package com.caotc.excel4j.config;

import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.constant.MenuNecessity;
import com.caotc.excel4j.constant.MenuType;
import com.caotc.excel4j.matcher.data.DataMatcher;
import com.caotc.excel4j.matcher.usermodel.StandardCellMatcher;
import com.caotc.excel4j.parse.result.Menu;
import com.caotc.excel4j.parse.result.StandardCell;
import com.google.common.base.Preconditions;

public class MenuConfig {
  public static class Builder {
    private TableConfig tableConfig;
    // 菜单匹配器
    private StandardCellMatcher menuMatcher;
    private DataMatcher dataMatcher;
    // 第一个数据单元格相对于菜单单元格的单元格距离
    private int distance;
    private MenuNecessity menuNecessity;
    private Direction direction;
    // 属性名字
    private String fieldName;
    private MenuType menuType;
    private MenuConfig parentMenuConfig;
    private MenuLoadConfig menuLoadConfig;

    public Builder tableConfig(TableConfig tableConfig) {
      this.tableConfig = tableConfig;
      return this;
    }

    public Builder menuMatcher(StandardCellMatcher menuMatcher) {
      this.menuMatcher = menuMatcher;
      return this;
    }

    public Builder dataMatcher(DataMatcher dataMatcher) {
      this.dataMatcher = dataMatcher;
      return this;
    }

    public Builder distance(int distance) {
      this.distance = distance;
      return this;
    }

    public Builder menuNecessity(MenuNecessity menuNecessity) {
      this.menuNecessity = menuNecessity;
      return this;
    }

    public Builder direction(Direction direction) {
      this.direction = direction;
      return this;
    }

    public Builder fieldName(String fieldName) {
      this.fieldName = fieldName;
      return this;
    }

    public Builder menuType(MenuType menuType) {
      this.menuType = menuType;
      return this;
    }

    public Builder parentMenuConfig(MenuConfig parentMenuConfig) {
      this.parentMenuConfig = parentMenuConfig;
      return this;
    }

    public Builder menuLoadConfig(MenuLoadConfig menuLoadConfig) {
      this.menuLoadConfig = menuLoadConfig;
      return this;
    }

    public MenuConfig build() {
      Preconditions.checkArgument(tableConfig != null || parentMenuConfig != null);
      Preconditions.checkNotNull(menuMatcher);
      Preconditions.checkNotNull(dataMatcher);
      Preconditions.checkNotNull(menuNecessity);
      Preconditions.checkArgument(direction != null || parentMenuConfig != null);
      Preconditions.checkNotNull(fieldName);
      Preconditions.checkNotNull(menuType);
      Preconditions.checkNotNull(menuLoadConfig);

      if (parentMenuConfig != null) {
        if (direction == null) {
          direction = parentMenuConfig.direction;
        }
        if (tableConfig == null) {
          tableConfig = parentMenuConfig.tableConfig;
        }
      }
      return new MenuConfig(this);
    }
  }

  private static final int DEFAULT_DISTANCE = 1;
  private static final MenuNecessity DEFAULT_MENUNECESSITY = MenuNecessity.MUST;

  public static Builder builder() {
    return new Builder().distance(DEFAULT_DISTANCE).menuNecessity(DEFAULT_MENUNECESSITY);
  }

  private final TableConfig tableConfig;
  // 菜单匹配器
  private final StandardCellMatcher menuMatcher;
  private final DataMatcher dataMatcher;
  // 第一个数据单元格相对于菜单单元格的单元格距离
  private final int distance;
  private final MenuNecessity menuNecessity;
  private final Direction direction;
  // 属性名字
  private final String fieldName;
  private final MenuType menuType;
  private final MenuConfig parentMenuConfig;
  private final MenuLoadConfig menuLoadConfig;

  public MenuConfig(Builder builder) {
    tableConfig = builder.tableConfig;
    menuMatcher = builder.menuMatcher;
    dataMatcher = builder.dataMatcher;
    distance = builder.distance;
    menuNecessity = builder.menuNecessity;
    direction = builder.direction;
    fieldName = builder.fieldName;
    menuType = builder.menuType;
    parentMenuConfig = builder.parentMenuConfig;
    menuLoadConfig = builder.menuLoadConfig;
  }

  public boolean isTopMenu() {
    return parentMenuConfig == null;
  }

  public void load(Menu menu) {
    menuLoadConfig.load(menu);
  }

  public boolean matches(StandardCell cell) {
    return menuMatcher.matches(cell);
  }

  public boolean isMustMenu() {
    return MenuNecessity.MUST.equals(getMenuNecessity());
  }

  public boolean isNotMustMenu() {
    return MenuNecessity.NOT_MUST.equals(getMenuNecessity());
  }

  public boolean isDataMenu() {
    return MenuType.DATA_MENU.equals(getMenuType());
  }

  public boolean isFixedDataMenu() {
    return isDataMenu() && LoadType.FIXED.equals(menuLoadConfig.getLoadType());
  }

  public boolean isUnFixedDataMenu() {
    return isDataMenu() && LoadType.UNFIXED.equals(menuLoadConfig.getLoadType());
  }

  public boolean isMixedDataMenu() {
    return isDataMenu() && LoadType.MIXED.equals(menuLoadConfig.getLoadType());
  }


  public MenuLoadConfig getMenuLoadConfig() {
    return menuLoadConfig;
  }

  public MenuType getMenuType() {
    return menuType;
  }

  public StandardCellMatcher getMenuMatcher() {
    return menuMatcher;
  }

  public DataMatcher getDataMatcher() {
    return dataMatcher;
  }

  public int getDistance() {
    return distance;
  }

  public String getFieldName() {
    return fieldName;
  }

  public MenuConfig getParentMenuConfig() {
    return parentMenuConfig;
  }

  public Direction getDirection() {
    return direction;
  }

  public MenuNecessity getMenuNecessity() {
    return menuNecessity;
  }

}
