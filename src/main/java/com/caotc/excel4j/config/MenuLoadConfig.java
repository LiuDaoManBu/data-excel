package com.caotc.excel4j.config;

import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.parse.result.Menu;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableSet;

public class MenuLoadConfig {
  public static class Builder {
    private MenuConfig menuConfig;
    private LoadType loadType;
    private com.google.common.collect.ImmutableSet.Builder<MenuConfig> childrenMenuConfigs=ImmutableSet.builder();
    private Integer dataNumber;
    
    public Builder menuConfig(MenuConfig menuConfig) {
      this.menuConfig=menuConfig;
      return this;
    }
    
    public Builder loadType(LoadType loadType) {
      this.loadType=loadType;
      return this;
    }
    
    public Builder addChildrenMenuConfig(MenuConfig config) {
      childrenMenuConfigs.add(config);
      return this;
    }
    
    public Builder dataNumber(Integer dataNumber) {
      this.dataNumber=dataNumber;
      return this;
    }
    
    public MenuLoadConfig build() {
      return new MenuLoadConfig(this);
    }
    
  }
  
  public static Builder builder() {
    return new Builder();
  }
  
  private final MenuConfig menuConfig;
  private final LoadType loadType;
  private final ImmutableCollection<MenuConfig> childrenMenuConfigs;
  private final Integer dataNumber;

  public MenuLoadConfig(Builder builder) {
    menuConfig=builder.menuConfig;
    loadType=builder.loadType;
    childrenMenuConfigs=builder.childrenMenuConfigs.build();
    dataNumber=builder.dataNumber;
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

  public Integer getDataNumber() {
    return dataNumber;
  }

}
