package com.caotc.excel4j.config;

import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.constant.MenuNecessity;
import com.caotc.excel4j.matcher.data.DataMatcher;
import com.caotc.excel4j.matcher.usermodel.StandardCellMatcher;
import com.caotc.excel4j.parse.result.Menu;

public class MenuConfig {
  // 菜单匹配器
  private StandardCellMatcher menuMatcher;
  private DataMatcher dataMatcher;
  // 第一个数据单元格相对于菜单单元格的单元格距离
  private Integer firstDistance;
  private MenuNecessity menuNecessity;
  private Direction direction;
  // 属性名字
  private String fieldName;
  private MenuConfig parentMenuConfig;
  private MenuLoadConfig menuLoadConfig;

  public boolean isTopMenu() {
    return parentMenuConfig == null;
  }

  public void load(Menu menu) {
    menuLoadConfig.load(menu);
  }
  
  public boolean isMustMenu() {
    return MenuNecessity.MUST.equals(getMenuNecessity());
  }
  
  public boolean isNotMustMenu() {
    return MenuNecessity.NOT_MUST.equals(getMenuNecessity());
  }
  
  
  
  public StandardCellMatcher getMenuMatcher() {
    return menuMatcher;
  }

  public void setMenuMatcher(StandardCellMatcher menuMatcher) {
    this.menuMatcher = menuMatcher;
  }

  public DataMatcher getDataMatcher() {
    return dataMatcher;
  }

  public void setDataMatcher(DataMatcher dataMatcher) {
    this.dataMatcher = dataMatcher;
  }

  public Integer getFirstDistance() {
    return firstDistance;
  }

  public void setFirstDistance(Integer firstDataDistance) {
    this.firstDistance = firstDataDistance;
  }

  public String getFieldName() {
    return fieldName;
  }

  public void setFieldName(String fieldName) {
    this.fieldName = fieldName;
  }

  public MenuConfig getParentMenuConfig() {
    return parentMenuConfig;
  }

  public void setParentMenuConfig(MenuConfig parentMenuConfig) {
    this.parentMenuConfig = parentMenuConfig;
  }

  public Direction getDirection() {
    return direction;
  }

  public void setDirection(Direction direction) {
    this.direction = direction;
  }

  public MenuNecessity getMenuNecessity() {
    return menuNecessity;
  }

  public void setMenuNecessity(MenuNecessity menuNecessity) {
    this.menuNecessity = menuNecessity;
  }

}
