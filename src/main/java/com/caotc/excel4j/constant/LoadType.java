package com.caotc.excel4j.constant;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import org.apache.commons.collections4.CollectionUtils;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.parse.result.Menu;
import com.caotc.excel4j.parse.result.StandardCell;

public enum LoadType {
  UNFIXED {
    @Override
    public void loadChildren(Menu menu) {
      MenuConfig config = menu.getCheckMenuConfig();
      List<StandardCell> menuCells =
          config.getDirection().getCells(menu.getCell(), config.getFirstDistance());
      menuCells.forEach(menuCell->{
        if(!menu.hasChildrenMenu(menuCell)) {
          menu.addChildrenMenu(new Menu(menuCell));
        }
      });
    }
  },
  FIXED {
    @Override
    public void loadChildren(Menu menu) {
      MenuConfig config = menu.getCheckMenuConfig();
      Collection<MenuConfig> childrenConfigs = config.getChildrenMenuConfigs();
      if (!CollectionUtils.isEmpty(childrenConfigs)) {
        List<StandardCell> menuCells =
            config.getDirection().getCells(menu.getCell(), config.getFirstDistance());
        if (!CollectionUtils.isEmpty(menuCells)) {
          childrenConfigs.forEach(childrenConfig -> {
            Optional<StandardCell> optional = menuCells.stream()
                .filter(menuCell -> childrenConfig.getMenuMatcher().matches(menuCell)).findAny();
            if (optional.isPresent()) {
              Menu childrenMenu = new Menu(optional.get(), childrenConfig);
              menu.addChildrenMenu(childrenMenu);
            }
          });
        }
      }
    }
  },MIXED {
    @Override
    public void loadChildren(Menu menu) {
      FIXED.loadChildren(menu);
      UNFIXED.loadChildren(menu);
    }
  };
  public abstract void loadChildren(Menu menu);
  public abstract void loadData(Menu menu);
}
