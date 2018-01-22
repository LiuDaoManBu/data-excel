package com.github.liudaomanbu.excel.parse.result;

import com.github.liudaomanbu.excel.config.MenuDataConfig;
import com.google.common.collect.ImmutableList;

public class MenuData<T> {
  private final Menu<T> menu;
  private final MenuDataConfig<T> config;
  private final ImmutableList<StandardCell> valueCells;

  public MenuData(Menu<T> menu) {
    super();
    this.menu = menu;
    this.config = menu.getConfig().getDataConfig();
    this.valueCells = menu.getChildrens().isEmpty() ? config.getLoadType().getDataCells(menu)
        : ImmutableList.of();
  }

  // public ImmutableList getCellValues() {
  // // cell不是必然有值,为null时ImmutableList会无法放入null而报错
  // return valueCells.stream().map(StandardCell::getValue).map(dataConfig::cast)
  // .collect(ImmutableList.toImmutableList());
  // }

  public Menu<T> getMenu() {
    return menu;
  }

  public MenuDataConfig<T> getConfig() {
    return config;
  }

  public ImmutableList<StandardCell> getValueCells() {
    return valueCells;
  }

}
