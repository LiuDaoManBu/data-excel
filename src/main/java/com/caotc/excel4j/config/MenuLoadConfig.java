package com.caotc.excel4j.config;

import com.caotc.excel4j.constant.LoadType;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableSet;

public class MenuLoadConfig {
  public static class Builder {
    private MenuConfig menuConfig;
    private LoadType loadType;
    private com.google.common.collect.ImmutableSet.Builder<MenuConfig> childrenMenuConfigs =
        ImmutableSet.builder();
    private Integer dataNumber;

    public MenuLoadConfig build() {
      return new MenuLoadConfig(this);
    }

    public MenuConfig getMenuConfig() {
      return menuConfig;
    }

    public Builder setMenuConfig(MenuConfig menuConfig) {
      this.menuConfig = menuConfig;
      return this;
    }

    public LoadType getLoadType() {
      return loadType;
    }

    public Builder setLoadType(LoadType loadType) {
      this.loadType = loadType;
      return this;
    }

    public com.google.common.collect.ImmutableSet.Builder<MenuConfig> getChildrenMenuConfigs() {
      return childrenMenuConfigs;
    }

    public Builder setChildrenMenuConfigs(
        com.google.common.collect.ImmutableSet.Builder<MenuConfig> childrenMenuConfigs) {
      this.childrenMenuConfigs = childrenMenuConfigs;
      return this;
    }

    public Integer getDataNumber() {
      return dataNumber;
    }

    public Builder setDataNumber(Integer dataNumber) {
      this.dataNumber = dataNumber;
      return this;
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
    menuConfig = builder.menuConfig;
    loadType = builder.loadType;
    childrenMenuConfigs = builder.childrenMenuConfigs.build();
    dataNumber = builder.dataNumber;
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
