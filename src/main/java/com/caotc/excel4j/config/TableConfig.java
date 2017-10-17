package com.caotc.excel4j.config;

import java.util.Collection;
import com.caotc.excel4j.constant.Direction;
import com.google.common.collect.Collections2;

public class TableConfig {
  public static class Builder {
    private SheetConfig sheetConfig;
    private Direction fixedMenuDirection;
    private Direction unFixedMenuDirection;
    private Collection<MenuConfig> menuConfigs;

    public TableConfig builder() {
      return new TableConfig(this);
    }

    public SheetConfig getSheetConfig() {
      return sheetConfig;
    }

    public Builder setSheetConfig(SheetConfig sheetConfig) {
      this.sheetConfig = sheetConfig;
      return this;
    }

    public Direction getFixedMenuDirection() {
      return fixedMenuDirection;
    }

    public Builder setFixedMenuDirection(Direction fixedMenuDirection) {
      this.fixedMenuDirection = fixedMenuDirection;
      return this;
    }

    public Direction getUnFixedMenuDirection() {
      return unFixedMenuDirection;
    }

    public Builder setUnFixedMenuDirection(Direction unFixedMenuDirection) {
      this.unFixedMenuDirection = unFixedMenuDirection;
      return this;
    }

    public Collection<MenuConfig> getMenuConfigs() {
      return menuConfigs;
    }

    public Builder setMenuConfigs(Collection<MenuConfig> menuConfigs) {
      this.menuConfigs = menuConfigs;
      return this;
    }
    
  }

  private final SheetConfig sheetConfig;
  private final Direction fixedMenuDirection;
  private final Direction unFixedMenuDirection;
  private final Collection<MenuConfig> menuConfigs;
  private final Collection<MenuConfig> topMenuConfigs;

  public TableConfig(Builder builder) {
    sheetConfig=builder.sheetConfig;
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
}
