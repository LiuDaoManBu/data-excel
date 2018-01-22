package com.github.liudaomanbu.excel.parse.result;

import com.github.liudaomanbu.excel.config.MenuDataConfig;
import com.google.common.collect.ImmutableList;

public class MenuData {
  private final Menu menu;
  private final MenuDataConfig config;
  private final ImmutableList<StandardCell> valueCells;

  public MenuData(Menu menu) {
    super();
    this.menu = menu;
    this.config = menu.getConfig().getDataConfig();
    this.valueCells = menu.getChildrens().isEmpty() ? config.getLoadType().getDataCells(menu)
        : ImmutableList.of();
  }

//  public ImmutableList getCellValues() {
//    // cell不是必然有值,为null时ImmutableList会无法放入null而报错
//    return valueCells.stream().map(StandardCell::getValue).map(dataConfig::cast)
//        .collect(ImmutableList.toImmutableList());
//  }

  public Menu getMenu() {
    return menu;
  }

  public MenuDataConfig getConfig() {
    return config;
  }

  public ImmutableList<StandardCell> getValueCells() {
    return valueCells;
  }

}
