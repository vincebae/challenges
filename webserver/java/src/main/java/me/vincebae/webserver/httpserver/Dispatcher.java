package me.vincebae.webserver.httpserver;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.net.Socket;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.util.Scanner;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** Dispatcher */
public class Dispatcher implements Runnable {

  private static final Logger LOGGER = LoggerFactory.getLogger(Dispatcher.class);

  private final BlockingQueue<Socket> blockingQueue;

  public Dispatcher(int size) {
    this.blockingQueue = new ArrayBlockingQueue<>(size);
  }

  @Override
  public void run() {
    LOGGER.info("Dispatcher::run");
    while (true) {
      try {
        Socket socket = blockingQueue.take();
        process(socket);
      } catch (InterruptedException e) {
      }
    }
  }

  public void dispatch(Socket socket) {
    try {
      blockingQueue.put(socket);
    } catch (InterruptedException e) {
    }
  }

  private void process(Socket socket) {
    LOGGER.info("socket taken from blocking queue.");
    try (var in =
            new BufferedReader(
                new InputStreamReader(socket.getInputStream(), StandardCharsets.UTF_8));
        var out =
            new PrintStream(
                new BufferedOutputStream(socket.getOutputStream()),
                false,
                StandardCharsets.UTF_8.toString())) {
      LOGGER.info("Read request...");
      final char[] buffer = new char[socket.getInputStream().available()];
      final int read = in.read(buffer);

      try (Scanner sc = new Scanner(new String(buffer))) {
        while (sc.hasNextLine()) {
          String line = sc.nextLine();
          LOGGER.info("read: {}", line);
        }
      }
      LOGGER.info("Read done.");

      LOGGER.info("Writing response...");
      String response = "Hello, World!";
      out.println("HTTP/1.0 200 OK");
      out.println("Content-Type: text/plain");
      out.println("Date: " + LocalDate.now());
      out.println("Content-length: " + response.length());
      out.println("");
      out.println(response);
      LOGGER.info("Writing done.");
    } catch (IOException e) {
    }
  }
}
