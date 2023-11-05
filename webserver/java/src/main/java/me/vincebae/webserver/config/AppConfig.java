package me.vincebae.webserver.config;

import com.google.common.annotations.VisibleForTesting;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.util.Properties;

/** Class to provide application configuration read from properties file in resources directory. */
public class AppConfig {

  // Property filename under resources directory.
  private static final String PROPERTY_FILE = "app.properties";

  // Create a singleton instance upfront.
  private static AppConfig singletonInstance;

  private final Properties properties;

  /**
   * Package private constructor to prevent instantiation beside the singleton instance except for
   * testing.
   */
  @VisibleForTesting
  AppConfig(Reader reader) throws IOException {
    if (singletonInstance != null) {
      throw new IllegalStateException("AppConfig instance already exists");
    }
    this.properties = new Properties();
    this.properties.load(reader);
  }

  public static AppConfig getInstance() {
    if (singletonInstance == null) {
      synchronized (AppConfig.class) {
        if (singletonInstance == null) {
          ClassLoader classLoader = AppConfig.class.getClassLoader();
          try (BufferedReader reader =
              new BufferedReader(
                  new InputStreamReader(
                      classLoader.getResourceAsStream(PROPERTY_FILE), StandardCharsets.UTF_8))) {
            singletonInstance = new AppConfig(reader);
          } catch (IOException e) {
            throw new IllegalStateException(
                "Failed to load application config from " + PROPERTY_FILE + ".");
          }
        }
      }
    }
    return singletonInstance;
  }

  public int propertyInt(String propertyName) {
    String property = property(propertyName);
    try {
      return Integer.parseInt(property);
    } catch (NumberFormatException e) {
      throw new IllegalArgumentException(
          String.format("Property %s is not an integer", propertyName));
    }
  }

  public String property(String propertyName) {
    String property = properties.getProperty(propertyName);
    if (property != null) {
      return property;
    } else {
      throw new IllegalArgumentException(String.format("Property %s doesn't exist", propertyName));
    }
  }
}
