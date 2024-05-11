package me.vincebae.webserver.httpserver;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.concurrent.ExecutorService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/** HttpServer */
public class HttpServer implements Runnable {

  private static final Logger LOGGER = LoggerFactory.getLogger(HttpServer.class);

  private final int port;
  private final int backlog;
  private final Dispatcher dispatcher;
  private final ExecutorService executorService;

  public HttpServer(int port, int backlog, Dispatcher dispatcher, ExecutorService executorService) {
    this.port = port;
    this.backlog = backlog;
    this.dispatcher = dispatcher;
    this.executorService = executorService;
  }

  @Override
  public void run() {
    LOGGER.info("HttpServer::run");
    try (var serverSocket = new ServerSocket(port, backlog)) {
      LOGGER.info("Server socket created at port {}", port);
      while (true) {
        Socket socket = serverSocket.accept();
        LOGGER.info("socket accepted and being sent to dispatcher");
        dispatcher.dispatch(socket);
      }
    } catch (IOException e) {
      LOGGER.error("Failed to create server socket at port {}", port);
    }
  }

  public void start() {
    executorService.execute(dispatcher);
    executorService.execute(this);
    LOGGER.info("Server started");
  }
}
