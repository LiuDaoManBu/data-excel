package com.caotc.excel4j.constant;

import com.caotc.excel4j.parse.result.Menu;

public enum MenuType {
  DATA_MENU {
    @Override
    public void load(Menu menu) {
      menu.getCheckMenuConfig().getLoadType().loadData(menu);
    }
  },NO_DATA_MENU {
    @Override
    public void load(Menu menu) {
      menu.getCheckMenuConfig().getLoadType().loadChildren(menu);      
    }
  };
  public abstract void load(Menu menu);
}
