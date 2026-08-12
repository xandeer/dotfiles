#!/usr/bin/env ruby

require "json"
require "socket"

log_path = ARGV.fetch(0)
server = TCPServer.new("127.0.0.1", 0)
mutex = Mutex.new
$stdout.sync = true
puts server.addr[1]

def reply(socket, status, headers, body)
  reason = { 200 => "OK", 302 => "Found" }.fetch(status)
  socket.write("HTTP/1.1 #{status} #{reason}\r\n")
  headers.merge("Content-Length" => body.bytesize.to_s,
                "Connection" => "close").each do |name, value|
    socket.write("#{name}: #{value}\r\n")
  end
  socket.write("\r\n#{body}")
rescue Errno::EPIPE, Errno::ECONNRESET, IOError
  nil
end

loop do
  client = server.accept
  Thread.new(client) do |socket|
    begin
      method, path, = socket.gets.to_s.split(" ")
      headers = {}
      while (line = socket.gets)
        line = line.sub(/\r?\n\z/, "")
        break if line.empty?

        name, value = line.split(":", 2)
        headers[name.downcase] = value.to_s.strip
      end
      body = socket.read(headers.fetch("content-length", "0").to_i)
      body.force_encoding(Encoding::UTF_8)
      event = { "method" => method, "path" => path,
                "authorization" => headers["authorization"],
                "content_type" => headers["content-type"], "body" => body }
      mutex.synchronize do
        File.open(log_path, "a") { |file| file.puts(JSON.generate(event)) }
      end

      case path
      when "/ok", "/stale"
        content = JSON.generate("candidate" => "network-ok")
        response = JSON.generate("choices" => [{ "message" => { "content" => content } }])
        reply(socket, 200, { "Content-Type" => "application/json" }, response)
      when "/redirect"
        reply(socket, 302, { "Location" => "/must-not-be-called" }, "")
      when "/slow"
        sleep 5
        reply(socket, 200, { "Content-Type" => "application/json" }, "{}")
      when "/large"
        reply(socket, 200, { "Content-Type" => "application/json" }, "x" * 65_537)
      when "/must-not-be-called"
        reply(socket, 200, { "Content-Type" => "application/json" }, "{}")
      else
        reply(socket, 200, { "Content-Type" => "application/json" }, "{}")
      end
    ensure
      socket.close unless socket.closed?
    end
  end
end
