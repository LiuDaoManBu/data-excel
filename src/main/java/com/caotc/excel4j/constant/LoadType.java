package com.caotc.excel4j.constant;

import java.util.List;
import java.util.Optional;
import com.caotc.excel4j.parse.result.Menu;
import com.caotc.excel4j.parse.result.StandardCell;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableList.Builder;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;

public enum LoadType {
  UNFIXED {
    @Override
    public <V> ImmutableList<StandardCell> getDataCells(Menu<V> menu) {
      ImmutableCollection<StandardCell> menuCells =
          menu.getTable().getMenus().map(Menu::getCell).collect(ImmutableSet.toImmutableSet());
      Builder<StandardCell> builder = ImmutableList.builder();
      //TODO stream
      for (Optional<StandardCell> optional = menu.nextDataCell(menu.getCell()); optional
          .filter(cell -> !menuCells.contains(cell))
          .isPresent(); optional = menu.nextDataCell(optional.get())) {
        builder.add(optional.get());
      }
      return builder.build();
    }

  },
  FIXED {
    @Override
    public <V> ImmutableList<StandardCell> getDataCells(Menu<V> menu) {
      ImmutableCollection<StandardCell> menuCells =
          menu.getTable().getMenus().map(Menu::getCell).collect(ImmutableSet.toImmutableSet());
      List<StandardCell> cells = Lists.newArrayList();

    //TODO stream
      for (Optional<StandardCell> optional = menu.nextDataCell(menu.getCell()); optional
          .filter(cell -> !menuCells.contains(cell)).isPresent()
          && cells.size() <= menu.getMenuConfig().getDataConfig().getDataNumber(); optional =
              menu.nextDataCell(optional.get())) {
        cells.add(optional.get());
      }
      //TODO data不够指定数目的error
      return ImmutableList.copyOf(cells);
    }

  }
  // ,MIXED {
  // @Override
  // public <V> ImmutableList<StandardCell> getDataCells(Menu<V> menu) {
  // return UNFIXED.getDataCells(menu);
  // }
  // }
  ;

  public abstract <V> ImmutableList<StandardCell> getDataCells(Menu<V> menu);
}
