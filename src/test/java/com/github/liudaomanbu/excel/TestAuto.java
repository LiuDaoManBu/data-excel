package com.github.liudaomanbu.excel;

//@AutoValue
//@ExcelTable
//abstract class Animal {
//  abstract String name();
//  abstract int numberOfLegs();
//  abstract Person person();
//
//  public static Animal create(String name, int numberOfLegs, Person person) {
//    return builder()
//        .name(name)
//        .numberOfLegs(numberOfLegs)
//        .person(person)
//        .build();
//  }
//
//  public static Builder builder() {
//    return new AutoValue_Animal.Builder();
//  }
//
//
//  @AutoValue.Builder
//  public abstract static class Builder {
//
//    public abstract Builder name(String name);
//
//    public abstract Builder numberOfLegs(int numberOfLegs);
//
//    public abstract Builder person(Person person);
//
//    public abstract Animal build();
//  }
//}
//
//@AutoValue
//abstract class Person{
//  abstract Animal animal();
//
//  public static Person create(Animal.Builder animal) {
//    return new AutoValue_Person(animal.build());
//  }
//
//}
public class TestAuto {

  public static void main(String[] args) {
//    System.out.println(Person.create(Animal.builder().name("sdfs").numberOfLegs(5)));
  }
}
