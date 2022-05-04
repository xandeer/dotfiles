#!/usr/bin/env elixir
url = "https://0x0.st"
path = System.argv |> hd
"Uploading #{path} to #{url} ..." |> IO.puts

res = "curl -sF file=@#{path} #{url}"
|> to_charlist
|> :os.cmd
|> to_string
|> String.trim

"You can visit it from #{res}" |> IO.puts

"echo #{res} | tr -d \"\n\" | pbcopy"
|> to_charlist
|> :os.cmd
