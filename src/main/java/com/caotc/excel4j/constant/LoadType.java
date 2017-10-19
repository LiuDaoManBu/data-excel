package com.caotc.excel4j.constant;

import java.util.Collection;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.parse.result.Menu;
import com.caotc.excel4j.parse.result.StandardCell;
import com.google.common.base.Optional;
import com.google.common.collect.Iterables;

public enum LoadType {
  UNFIXED {
    @Override
    public void loadChildren(Menu menu) {
      MenuConfig config = menu.getCheckMenuConfig();
      List<StandardCell> menuCells =
          config.getDirection().get(menu.getCell(), config.getDistance());
      menuCells.forEach(menuCell -> {
        if (!menu.hasChildrenMenu(menuCell)) {
          menu.addChildrenMenu(Menu.builder().setCell(menuCell).setParentMenu(menu).build());
        }
      });
    }
  },
  FIXED {
    @Override
    public void loadChildren(Menu menu) {
      MenuConfig config = menu.getCheckMenuConfig();
      Collection<MenuConfig> childrenConfigs = config.getMenuLoadConfig().getChildrenMenuConfigs();
      if (!CollectionUtils.isEmpty(childrenConfigs)) {
        List<StandardCell> menuCells =
            config.getDirection().get(menu.getCell(), config.getDistance());
        if (!CollectionUtils.isEmpty(menuCells)) {
          childrenConfigs.forEach(childrenConfig -> {
            Optional<StandardCell> optional = Iterables.tryFind(menuCells, childrenConfig::matches);
            if (optional.isPresent()) {
              menu.addChildrenMenu(Menu.builder().setCell(optional.get()).setMenuConfig(childrenConfig)
                  .setParentMenu(menu).build());
            }
          });
        }
      }
    }
  },
  MIXED {
    @Override
    public void loadChildren(Menu menu) {
      FIXED.loadChildren(menu);
      UNFIXED.loadChildren(menu);
    }
  };
  public abstract void loadChildren(Menu menu);
}
