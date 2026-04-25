#!/usr/bin/env bb

(require '[babashka.curl :as curl]
         '[clojure.string :as str]
         '[clojure.java.io :as io]
         '[clojure.java.shell :as shell])

;; Function to load environment variables from a .env file
(defn load-env [filepath]
  (when (.exists (io/file filepath))
    (with-open [rdr (io/reader filepath)]
      (doseq [line (line-seq rdr)]
        (let [trimmed (str/trim line)]
          (when (and (not (str/starts-with? trimmed "#"))
                     (str/includes? trimmed "="))
            (let [[k v] (str/split trimmed #"=" 2)]
              (System/setProperty (str/trim k) (str/trim v)))))))))

;; Load .env file
(load-env ".env")

;; Configuration
(def subscription-key (or (System/getProperty "AZURE_TTS_KEY")
                         (System/getenv "AZURE_TTS_KEY")
                         (throw (ex-info "AZURE_TTS_KEY not set" {}))))
(def region (or (System/getProperty "AZURE_TTS_REGION")
               (System/getenv "AZURE_TTS_REGION")
               (throw (ex-info "AZURE_TTS_REGION not set" {}))))
(def endpoint (str "https://" region ".tts.speech.microsoft.com/cognitiveservices/v1"))

;; Define a map of languages to voice names
(def language-voice-map
  {"zh-CN" "zh-CN-XiaochenNeural"  ; Simplified Chinese
   "en-US" "en-US-MichelleNeural"})    ; English (US)

(defn detect-language [text]
  (cond
    (re-find #"\p{InCJK_Unified_Ideographs}" text) "zh-CN"
    :else "en-US"))

;; Function to generate SSML
(defn generate-ssml [text language name]
  (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
       "<speak version=\"1.0\" xml:lang=\"en-US\">"
       "<voice "
       "xml:lang=\"" language "\""
       ;; ;; xml:gender=\"Female\" name=\"en-US-AriaNeural\">"
       " name=\"" name "\">"
       text
       "</voice></speak>"))

;; Function to make the TTS request
(defn synthesize-speech [text output-file language voice-name]
  (let [ssml (generate-ssml text language voice-name)
        headers {"Content-Type" "application/ssml+xml"
                 "X-Microsoft-OutputFormat" "audio-48khz-192kbitrate-mono-mp3"
                 "Ocp-Apim-Subscription-Key" subscription-key}
        response (curl/post endpoint
                            {:headers headers
                             :body ssml
                             :as :bytes})]
    (if (= 200 (:status response))
      (do
        (with-open [out (io/output-stream output-file)]
          (.write out (:body response)))
        (println "Audio saved to" output-file)
        output-file)
      (do
        (println "Error:" (:status response))
        (println (String. (:body response)))))))

(defn copy-to-clipboard [filepath]
  (let [absolute-path (.getAbsolutePath (io/file filepath))
        os (System/getProperty "os.name")]
    (cond
      ;; macOS
      (str/includes? os "Mac")
      (do
        (println "Copying to clipboard using pbcopy..." absolute-path)
        (let [pb (ProcessBuilder. (into-array ["pbcopy"]))
              p (.start pb)
              out (.getOutputStream p)]
          (with-open [writer (io/writer out)]
            (.write writer absolute-path))
          (.waitFor p)))

      ;; Linux
      (str/includes? os "Linux")
      (do
        (println "Copying path to clipboard using xclip...")
        ;; Try xclip first, then xsel
        (if (zero? (:exit (clojure.java.shell/sh "which" "xclip")))
          (clojure.java.shell/sh "xclip" "-selection" "clipboard" :in absolute-path)
          (if (zero? (:exit (clojure.java.shell/sh "which" "xsel")))
            (clojure.java.shell/sh "xsel" "--clipboard" "--input" :in absolute-path)
            (println "Neither xclip nor xsel is installed. Cannot copy to clipboard."))))

      ;; Windows
      (str/includes? os "Windows")
      (do
        (println "Copying path to clipboard using clip...")
        (clojure.java.shell/sh "cmd" "/c" "echo" absolute-path "| clip"))

      ;; Unsupported OS
      :else
      (println "Unsupported OS for clipboard operations."))))

(defn play-audio [filepath]
  (let [absolute-path (.getAbsolutePath (io/file filepath))
        os (System/getProperty "os.name")]
    (cond
      ;; macOS
      (str/includes? os "Mac")
      (do
        (println "Playing audio using afplay...")
        (clojure.java.shell/sh "afplay" absolute-path))

      ;; Linux
      (str/includes? os "Linux")
      (do
        (println "Playing audio using aplay, mpg123, or paplay...")
        (cond
          ;; Try aplay
          (zero? (:exit (clojure.java.shell/sh "which" "aplay")))
          (clojure.java.shell/sh "aplay" absolute-path)

          ;; Try mpg123
          (zero? (:exit (clojure.java.shell/sh "which" "mpg123")))
          (clojure.java.shell/sh "mpg123" absolute-path)

          ;; Try paplay
          (zero? (:exit (clojure.java.shell/sh "which" "paplay")))
          (clojure.java.shell/sh "paplay" absolute-path)

          ;; None found
          :else
          (println "No suitable audio player found. Please install aplay, mpg123, or paplay.")))

    ;; Windows
    (str/includes? os "Windows")
    (do
      (println "Playing audio using start command...")
      (clojure.java.shell/sh "cmd" "/c" "start" "" absolute-path))

    ;; Unsupported OS
    :else
    (println "Unsupported OS for audio playback."))))

(defn md5-hash [s]
  (let [digest (java.security.MessageDigest/getInstance "MD5")
        bytes (.getBytes s "UTF-8")]
    (apply str (map #(format "%02x" %) (.digest digest bytes)))))

(defn file-exists? [filepath]
  (.exists (io/file filepath)))

(defn parse-args [args]
  (loop [remaining args
         options {:output-dir "."
                  :language "en-US"
                  :voice-name "en-US-JennyNeural"
                  :text nil}]
    (if (empty? remaining)
      options
      (let [[arg & rest-args] remaining]
        (cond
          (= arg "-d") (recur (rest rest-args) (assoc options :output-dir (first rest-args)))
          (= arg "-l") (recur (rest rest-args) (assoc options :language (first rest-args)))
          (= arg "-n") (recur (rest rest-args) (assoc options :voice-name (first rest-args)))
          :else (recur rest-args (assoc options :text (str (:text options) " " arg))))))))

(defn generate-output-file [text output-dir]
  (let [hash (md5-hash text)
        filename (str hash ".mp3")]
    (io/file output-dir filename)))

(defn -main [& args]
  (let [{:keys [output-dir language voice-name text]} (parse-args args)
        text (or text "Hello, this is a test of Azure Text-to-Speech.")
        language (or language (detect-language text))
        voice-name (or voice-name (get language-voice-map language))
        output-file (generate-output-file text output-dir)]
    (if (file-exists? output-file)
      (do
        (println "Audio file already exists:" (.getPath output-file))
        (copy-to-clipboard (.getPath output-file))
        (play-audio (.getPath output-file)))
      (do
        (println "Audio file does not exist. Synthesizing speech...")
        (let [saved-file (synthesize-speech text (.getPath output-file) language voice-name)]
          (println "Saved file:" saved-file)
          (when saved-file
            (copy-to-clipboard saved-file)
            (play-audio saved-file)))))))

;; 这个更新后的版本允许用户通过命令行参数自定义TTS设置：
;; -d：指定音频文件的存储路径
;; -l：指定要使用的语言
;; -n：指定要使用的语音名称
;; 文本可以在任何参数之前或之后提供
;; 例如：clj -m tts.core -d "/path/to/output" -l "zh-CN" -n "zh-CN-XiaoxiaoNeural" "你好世界"


;; Run the main function with command-line arguments
(apply -main *command-line-args*)
