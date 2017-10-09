package com.caotc.excel4j.config;

import java.util.Collection;
import com.caotc.excel4j.constant.Direction;
import com.google.common.collect.Collections2;

public class TableConfig {
  private final Direction fixedMenuDirection;
  private final Direction unFixedMenuDirection;
  private final Collection<MenuConfig> menuConfigs;
  private final Collection<MenuConfig> topMenuConfigs=Collections2.filter(menuConfigs, MenuConfig::isTopMenu);
  
  
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
