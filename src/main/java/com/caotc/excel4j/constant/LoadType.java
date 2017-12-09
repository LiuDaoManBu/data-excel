package com.caotc.excel4j.constant;

import java.util.List;
import java.util.Optional;
import com.caotc.excel4j.parse.result.Menu;
import com.caotc.excel4j.parse.result.StandardCell;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableList.Builder;
import com.google.common.collect.Lists;

public enum LoadType {
  UNFIXED {
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
    public ImmutableList<StandardCell> getDataCells(Menu menu) {
      List<StandardCell> cells = Lists.newArrayList();

      for (Optional<StandardCell> optional = Optional.of(menu.getCell()); optional.isPresent()
          && cells.size() <= menu.getMenuConfig().getDataConfig().getDataNumber(); optional =
              menu.nextDataCell(optional.get())) {
        cells.add(optional.get());
      }
      return ImmutableList.copyOf(cells);
    }

  },
  MIXED {
    @Override
    public ImmutableList<StandardCell> getDataCells(Menu menu) {
      return UNFIXED.getDataCells(menu);
    }
  };

  public abstract ImmutableList<StandardCell> getDataCells(Menu menu);
}
