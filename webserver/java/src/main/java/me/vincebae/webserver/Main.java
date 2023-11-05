package me.vincebae.webserver;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadLocalRandom;

import me.vincebae.webserver.config.AppConfig;
import me.vincebae.webserver.httpserver.Dispatcher;
import me.vincebae.webserver.httpserver.HttpServer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Main {

  private static final Logger LOGGER = LoggerFactory.getLogger(Main.class);

  private static class Printer implements Runnable {
    @Override
    public void run() {
      ThreadLocalRandom random = ThreadLocalRandom.current();
      for (int i = 0; i < 100; i++) {
        LOGGER.info("i = {}", i);
        try {
          Thread.sleep(500 + random.nextInt(500));
        } catch (Exception e) {
        }
      }
    }
  }

  public static void main(String[] args) {
    LOGGER.info("Start main.");
    AppConfig appConfig = AppConfig.getInstance();

    int threads = appConfig.propertyInt("threads");
    ExecutorService executorService = Executors.newFixedThreadPool(threads);

    int port = appConfig.propertyInt("port");
    int backlog = appConfig.propertyInt("backlog");
    Dispatcher dispatcher = new Dispatcher(backlog);
    HttpServer httpServer = new HttpServer(port, backlog, dispatcher, executorService);
    httpServer.start();
    LOGGER.info("End main.");

    // Printer printer = new Printer();
    // for (int i = 0; i < 10; i++) {
    //   executorService.execute(printer);
    // }
  }
}
