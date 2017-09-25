package com.caotc.excel4j.config;

import java.util.Collection;
import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.constant.MenuNecessity;
import com.caotc.excel4j.matcher.data.DataMatcher;
import com.caotc.excel4j.matcher.data.value.StringValueMatcher.StringValueMatcherType;
import com.caotc.excel4j.matcher.usermodel.StandardCellMatcher;

public class MenuConfig {
  private static final StringValueMatcherType DEFAULT_STRING_MATCH_TYPE =
      StringValueMatcherType.EQUALS;
  private static final Boolean DEFAULT_MUST = Boolean.TRUE;
  // 菜单匹配器
  private StandardCellMatcher menuMatcher;
  private DataMatcher dataMatcher;
  // 第一个数据单元格相对于菜单单元格的单元格距离
  private Integer firstDistance;
  private LoadType menuDataLoadType;
  private MenuNecessity menuNecessity;
  private LoadType childrenMenuLoadType;
  // 属性名字
  private String fieldName;
  private MenuConfig parentMenuConfig;
  private Collection<MenuConfig> childrenMenuConfigs;

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

  public Collection<MenuConfig> getChildrenMenuConfigs() {
    return childrenMenuConfigs;
  }

  public void setChildrenMenuConfigs(Collection<MenuConfig> childrenMenuConfigs) {
    this.childrenMenuConfigs = childrenMenuConfigs;
  }

  public LoadType getMenuDataLoadType() {
    return menuDataLoadType;
  }

  public void setMenuDataLoadType(LoadType menuDataLoadType) {
    this.menuDataLoadType = menuDataLoadType;
  }

  public MenuNecessity getMenuNecessity() {
    return menuNecessity;
  }

  public void setMenuNecessity(MenuNecessity menuNecessity) {
    this.menuNecessity = menuNecessity;
  }

  public LoadType getChildrenMenuLoadType() {
    return childrenMenuLoadType;
  }

  public void setChildrenMenuLoadType(LoadType childrenMenuLoadType) {
    this.childrenMenuLoadType = childrenMenuLoadType;
  }

}
