package com.caotc.excel4j.config;

import java.util.Collection;
import com.caotc.excel4j.constant.Direction;
import com.google.common.collect.Collections2;

public class TableConfig {
  public static class Builder {
    private Direction fixedMenuDirection;
    private Direction unFixedMenuDirection;
    private Collection<MenuConfig> menuConfigs;

    public Builder fixedMenuDirection(Direction fixedMenuDirection) {
      this.fixedMenuDirection = fixedMenuDirection;
      return this;
    }

    public Builder unFixedMenuDirection(Direction unFixedMenuDirection) {
      this.unFixedMenuDirection = unFixedMenuDirection;
      return this;
    }

    public TableConfig builder() {
      return new TableConfig(this);
    }
  }

  private final Direction fixedMenuDirection;
  private final Direction unFixedMenuDirection;
  private final Collection<MenuConfig> menuConfigs;
  private final Collection<MenuConfig> topMenuConfigs;

  public TableConfig(Builder builder) {
    fixedMenuDirection = builder.fixedMenuDirection;
    unFixedMenuDirection = builder.unFixedMenuDirection;
    menuConfigs = builder.menuConfigs;

    topMenuConfigs = Collections2.filter(menuConfigs, MenuConfig::isTopMenu);
  }

  public Collection<MenuConfig> getTopMenuConfigs() {
    return topMenuConfigs;
  }

  public Direction getFixedMenuDirection() {
    return fixedMenuDirection;
  }

  public Direction getUnFixedMenuDirection() {
    return unFixedMenuDirection;
  }

  public Collection<MenuConfig> getMenuConfigs() {
    return menuConfigs;
  }
}
