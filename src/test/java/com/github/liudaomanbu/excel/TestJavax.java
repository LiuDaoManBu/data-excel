package com.github.liudaomanbu.excel;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Set;
import javax.validation.ConstraintViolation;
import javax.validation.Validation;
import javax.validation.Validator;
import javax.validation.ValidatorFactory;
import javax.validation.constraints.AssertTrue;
import javax.validation.constraints.DecimalMax;
import javax.validation.constraints.DecimalMin;
import javax.validation.constraints.Future;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

class Student {


  @NotNull
  private String name;

  @Size(min = 6, max = 30, message = "地址应该在6-30字符之间")
  private String address;

  @DecimalMax(value = "100.00", message = "体重有些超标哦")
  @DecimalMin(value = "60.00", message = "多吃点饭吧")
  private BigDecimal weight;

  private String friendName;

  @AssertTrue
  private Boolean isHaveFriend() {
    return friendName != null ? true : false;
  }

  @Future(message = "生日必须在当前实践之前")
  private Date birthday;

  @Pattern(regexp = "^(.+)@(.+)$", message = "邮箱的格式不合法")
  private String email;


  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getAddress() {
    return address;
  }

  public void setAddress(String address) {
    this.address = address;
  }

  public BigDecimal getWeight() {
    return weight;
  }

  public void setWeight(BigDecimal weight) {
    this.weight = weight;
  }

  public String getFriendName() {
    return friendName;
  }

  public void setFriendName(String friendName) {
    this.friendName = friendName;
  }

  public Date getBirthday() {
    return birthday;
  }

  public void setBirthday(Date birthday) {
    this.birthday = birthday;
  }

  public String getEmail() {
    return email;
  }

  public void setEmail(String email) {
    this.email = email;
  }
}


public class TestJavax {
//  private static ValidatorFactory factory = Validation.buildDefaultValidatorFactory();

  public static void main(String[] args) {
  }

  public static void testValidor() {
    Student student = getBean();
  }

  private static Student getBean() {
    Student bean = new Student();
    bean.setName(null);
    bean.setAddress("北京");
    bean.setBirthday(new Date());
    bean.setFriendName(null);
    bean.setWeight(new BigDecimal(30));
    bean.setEmail("xiaogangfan163.com");
    return bean;
  }

//  public static <T> List<String> validate(T t) {
//    Validator validator = factory.getValidator();
//    Set<ConstraintViolation<T>> constraintViolations = validator.validate(t);
//
//    List<String> messageList = new ArrayList<>();
//    for (ConstraintViolation<T> constraintViolation : constraintViolations) {
//      messageList.add(constraintViolation.getMessage());
//      System.out.println();
//      System.out.println("ExecutableReturnValue:" + constraintViolation.getExecutableReturnValue());
//      System.out.println("InvalidValue:" + constraintViolation.getInvalidValue());
//      // 方法含义?
//      // System.out.println("LeafBean:" + constraintViolation.getLeafBean());
//      System.out.println("Message:" + constraintViolation.getMessage());
//      System.out.println("MessageTemplate:" + constraintViolation.getMessageTemplate());
//      System.out.println("ConstraintDescriptor:" + constraintViolation.getConstraintDescriptor());
//      System.out.println(
//          "ExecutableParameters:" + Arrays.toString(constraintViolation.getExecutableParameters()));
//      System.out.println("PropertyPath:" + constraintViolation.getPropertyPath());
//      // System.out.println("RootBean:" + constraintViolation.getRootBean());
//      // System.out.println("RootBeanClass:" + constraintViolation.getRootBeanClass());
//      System.out.println();
//    }
//    return messageList;
//  }
}
