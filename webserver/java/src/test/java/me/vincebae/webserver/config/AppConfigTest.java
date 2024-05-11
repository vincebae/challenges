package me.vincebae.webserver.config;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.io.StringReader;
import java.util.Arrays;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/** Unit test for AppConfig. */
class AppConfigTest {

  private static final String TEST_PROPERTIES =
      String.join("\n", Arrays.asList("property.int: 12345", "property.string: propertyValue"));

  private AppConfig subject;

  @BeforeEach
  void beforeEach() {
    try (var reader = new StringReader(TEST_PROPERTIES)) {
      subject = new AppConfig(reader);
    } catch (IOException e) {
      fail("IOException while reading properties");
    }
  }

  @Test
  void property_returnsResults() {
    assertThat(subject.property("property.int"), is("12345"));
    assertThat(subject.property("property.string"), is("propertyValue"));
  }

  @Test
  void property_propertyNotExist_throwsException() {
    assertThrows(IllegalArgumentException.class, () -> subject.property("property.not"));
  }

  @Test
  void propertyInt_returnsResults() {
    assertThat(subject.propertyInt("property.int"), is(12345));
  }

  @Test
  void propertyInt_propertyValueIsNotInt_throwsException() {
    assertThrows(IllegalArgumentException.class, () -> subject.propertyInt("property.string"));
  }

  @Test
  void propertyInt_propertyNotExist_throwsException() {
    assertThrows(IllegalArgumentException.class, () -> subject.propertyInt("property.not"));
  }
}
