package com.github.liudaomanbu.excel;

import com.google.auto.value.AutoValue;

@AutoValue
abstract class Animal {
//  static Animal create(String name, int numberOfLegs) {
//    // See "How do I...?" below for nested classes.
//    return new AutoValue_Animal(name, numberOfLegs);
//  }

  abstract String name();
  abstract int numberOfLegs();

  public static Animal create(String name, int numberOfLegs) {
    return new AutoValue_Animal(name, numberOfLegs);
  }
}
public class TestAuto {

}
