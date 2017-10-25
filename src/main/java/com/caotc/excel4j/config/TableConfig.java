package com.caotc.excel4j.config;

import java.util.Collection;
import com.caotc.excel4j.constant.Direction;
import com.google.common.collect.Collections2;

public class TableConfig<T> {
  public static class Builder<T> {
    private SheetConfig sheetConfig;
    private Direction fixedMenuDirection;
    private Direction unFixedMenuDirection;
    private Collection<MenuConfig> menuConfigs;

    public TableConfig<T> builder() {
      return new TableConfig<T>(this);
    }

    public SheetConfig getSheetConfig() {
      return sheetConfig;
    }

    public Builder<T> setSheetConfig(SheetConfig sheetConfig) {
      this.sheetConfig = sheetConfig;
      return this;
    }

    public Direction getFixedMenuDirection() {
      return fixedMenuDirection;
    }

    public Builder<T> setFixedMenuDirection(Direction fixedMenuDirection) {
      this.fixedMenuDirection = fixedMenuDirection;
      return this;
    }

    public Direction getUnFixedMenuDirection() {
      return unFixedMenuDirection;
    }

    public Builder<T> setUnFixedMenuDirection(Direction unFixedMenuDirection) {
      this.unFixedMenuDirection = unFixedMenuDirection;
      return this;
    }

    public Collection<MenuConfig> getMenuConfigs() {
      return menuConfigs;
    }

    public Builder<T> setMenuConfigs(Collection<MenuConfig> menuConfigs) {
      this.menuConfigs = menuConfigs;
      return this;
    }
    
  }

  private final SheetConfig sheetConfig;
  private final Class<T> type;
  private final Direction fixedMenuDirection;
  private final Direction unFixedMenuDirection;
  private final Collection<MenuConfig> menuConfigs;
  private final Collection<MenuConfig> topMenuConfigs;

  public TableConfig(Builder<T> builder) {
    sheetConfig=builder.sheetConfig;
    //TODO
    type=null;
    fixedMenuDirection = builder.fixedMenuDirection;
    unFixedMenuDirection = builder.unFixedMenuDirection;
    menuConfigs = builder.menuConfigs;

    topMenuConfigs = Collections2.filter(menuConfigs, MenuConfig::isTopMenu);
  }
  
  public SheetConfig getSheetConfig() {
    return sheetConfig;
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

  public Class<T> getType() {
    return type;
  }
  
}
