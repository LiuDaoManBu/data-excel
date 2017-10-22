package com.caotc.excel4j.constant;

import java.util.Collection;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.parse.result.Menu;
import com.caotc.excel4j.parse.result.StandardCell;
import com.google.common.base.Optional;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableList.Builder;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

public enum LoadType {
  UNFIXED {
    @Override
    public ImmutableList<Menu> getChildrenMenus(Menu menu) {
      Builder<Menu> builder = ImmutableList.builder();
      MenuConfig config = menu.getCheckMenuConfig();
      List<StandardCell> menuCells =
          config.getDirection().get(menu.getCell(), config.getDistance());
      menuCells.forEach(menuCell -> {
        builder.add(Menu.builder().setCell(menuCell).setParentMenu(menu).build());
      });
      return builder.build();
    }

    @Override
    public ImmutableList<StandardCell> getDataCells(Menu menu) {
      Builder<StandardCell> builder = ImmutableList.builder();
      for (Optional<StandardCell> optional = Optional.of(menu.getCell()); optional
          .isPresent(); optional = menu.nextDataCell(optional.get())) {
        builder.add(optional.get());
      }
      return builder.build();
    }

  },
  FIXED {
    @Override
    public ImmutableList<Menu> getChildrenMenus(Menu menu) {
      Builder<Menu> builder = ImmutableList.builder();

      MenuConfig config = menu.getCheckMenuConfig();
      Collection<MenuConfig> childrenConfigs = config.getMenuLoadConfig().getChildrenMenuConfigs();
      if (!CollectionUtils.isEmpty(childrenConfigs)) {
        List<StandardCell> menuCells =
            config.getDirection().get(menu.getCell(), config.getDistance());
        if (!CollectionUtils.isEmpty(menuCells)) {
          childrenConfigs.forEach(childrenConfig -> {
            Optional<StandardCell> optional = Iterables.tryFind(menuCells, childrenConfig::matches);
            if (optional.isPresent()) {
              builder.add(Menu.builder().setCell(optional.get()).setMenuConfig(childrenConfig)
                  .setParentMenu(menu).build());
            }
          });
        }
      }
      return builder.build();
    }

    @Override
    public ImmutableList<StandardCell> getDataCells(Menu menu) {
      List<StandardCell> cells = Lists.newArrayList();

      for (Optional<StandardCell> optional = Optional.of(menu.getCell()); optional.isPresent()
          && cells.size() <= menu.getCheckMenuConfig().getMenuLoadConfig()
              .getDataNumber(); optional = menu.nextDataCell(optional.get())) {
        cells.add(optional.get());
      }
      return ImmutableList.copyOf(cells);
    }

  },
  MIXED {
    @Override
    public ImmutableList<Menu> getChildrenMenus(Menu menu) {
      Builder<Menu> builder = ImmutableList.builder();
      MenuConfig config = menu.getCheckMenuConfig();
      List<StandardCell> menuCells =
          config.getDirection().get(menu.getCell(), config.getDistance());
      Collection<MenuConfig> childrenConfigs = config.getMenuLoadConfig().getChildrenMenuConfigs();
      menuCells.forEach(menuCell -> {
        com.caotc.excel4j.parse.result.Menu.Builder menuBuilder =
            Menu.builder().setCell(menuCell).setParentMenu(menu);
        Optional<MenuConfig> optional =
            Iterables.tryFind(childrenConfigs, childrenConfig -> childrenConfig.matches(menuCell));
        if (optional.isPresent()) {
          menuBuilder.setMenuConfig(optional.get());
        }
        builder.add(menuBuilder.build());
      });
      return builder.build();
    }

    @Override
    public ImmutableList<StandardCell> getDataCells(Menu menu) {
      return UNFIXED.getDataCells(menu);
    }
  };
  public abstract ImmutableList<Menu> getChildrenMenus(Menu menu);

  public abstract ImmutableList<StandardCell> getDataCells(Menu menu);
}
