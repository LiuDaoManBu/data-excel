package com.caotc.excel4j.config;

import com.caotc.excel4j.constant.LoadType;
import com.google.common.collect.ImmutableCollection;

public class MenuLoadConfig {
  private final LoadType loadType;
  private final ImmutableCollection<MenuConfig> childrenMenuConfigs;
  
  public MenuLoadConfig(LoadType loadType, ImmutableCollection<MenuConfig> childrenMenuConfigs) {
    super();
    this.loadType = loadType;
    this.childrenMenuConfigs = childrenMenuConfigs;
  }

  
  
  public LoadType getLoadType() {
    return loadType;
  }

  public ImmutableCollection<MenuConfig> getChildrenMenuConfigs() {
    return childrenMenuConfigs;
  }
  
}
