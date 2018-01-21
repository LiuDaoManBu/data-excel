package com.github.liudaomanbu.excel.constant;

import java.util.Collections;
import java.util.List;
import java.util.Optional;
import com.github.liudaomanbu.excel.parse.result.Menu;
import com.github.liudaomanbu.excel.parse.result.StandardCell;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableList.Builder;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;

public enum LoadType {
  UNFIXED {
    @Override
    public  ImmutableList<StandardCell> getDataCells(Menu menu) {
      // TODO 此时menu尚未存在table中
      // ImmutableCollection<StandardCell> menuCells =
      // menu.getTable().getMenus().map(Menu::getCell).collect(ImmutableSet.toImmutableSet());
      Builder<StandardCell> builder = ImmutableList.builder();
      // TODO stream
      for (Optional<StandardCell> optional = menu.nextDataCell(menu.getCell()); optional
          // .filter(cell -> !menuCells.contains(cell))
          .isPresent(); optional = menu.nextDataCell(optional.get())) {
        builder.add(optional.get());
      }
      return builder.build();
    }

  },
//  FIXED {
//    @Override
//    public  ImmutableList<StandardCell> getDataCells(Menu menu) {
//      // ImmutableCollection<StandardCell> menuCells =
//      // menu.getTable().getMenus().map(Menu::getCell).collect(ImmutableSet.toImmutableSet());
//      List<StandardCell> cells = Lists.newArrayList();
//
//      // TODO stream
//      for (Optional<StandardCell> optional = menu.nextDataCell(menu.getCell()); optional
//          // .filter(cell -> !menuCells.contains(cell))
//          .isPresent()
//          && cells.size() <= menu.getMenuConfig().getDataConfig().getDataNumber(); optional =
//              menu.nextDataCell(optional.get())) {
//        cells.add(optional.get());
//      }
//      // TODO data不够指定数目的error
//      return ImmutableList.copyOf(cells);
//    }
//
//  },
  SINGLE {
    @Override
    public  ImmutableList<StandardCell> getDataCells(Menu menu) {
      return ImmutableList.of(menu.nextDataCell(menu.getCell()).get());
    }
  };

  public abstract  ImmutableList<StandardCell> getDataCells(Menu menu);
}
