package com.caotc.excel4j.parse.result;

import java.util.Optional;
import com.caotc.excel4j.config.MenuDataConfig;
import com.google.common.collect.ImmutableList;
import com.caotc.excel4j.parse.error.ConstraintViolation;

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
//    // TODO
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
