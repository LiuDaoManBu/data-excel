package com.caotc.excel4j.config;

import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.parse.result.Menu;
import com.google.common.collect.ImmutableCollection;

public class MenuLoadConfig {
  private final MenuConfig menuConfig;
  private final LoadType loadType;
  private final ImmutableCollection<MenuConfig> childrenMenuConfigs;

  public MenuLoadConfig(MenuConfig menuConfig, LoadType loadType,
      ImmutableCollection<MenuConfig> childrenMenuConfigs) {
    super();
    this.menuConfig = menuConfig;
    this.loadType = loadType;
    this.childrenMenuConfigs = childrenMenuConfigs;
  }

  public void load(Menu menu) {
    if(!menuConfig.isDataMenu()) {
      loadType.loadChildren(menu);
    }
  }
  
  public MenuConfig getMenuConfig() {
    return menuConfig;
  }

  public LoadType getLoadType() {
    return loadType;
  }

  public ImmutableCollection<MenuConfig> getChildrenMenuConfigs() {
    return childrenMenuConfigs;
  }
  
}
