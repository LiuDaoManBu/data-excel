package com.caotc.excel4j.constant;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.CellRangeAddressBase;
import com.caotc.excel4j.parse.result.StandardCell;
import com.caotc.excel4j.util.ExcelUtil;
import com.google.common.base.Preconditions;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

public enum Direction {
  TOP {
    @Override
    public Direction getNegativeDirection() {
      return BOTTOM;
    }

    @Override
    public CellRangeAddress nextAddress(CellRangeAddressBase address) {
      Preconditions.checkNotNull(address);
      return new CellRangeAddress(address.getFirstRow() - DEFAULT_DISTANCE,
          address.getFirstRow() - DEFAULT_DISTANCE, address.getFirstColumn(),
          address.getLastColumn());
    }
  },
  BOTTOM {
    @Override
    public Direction getNegativeDirection() {
      return TOP;
    }

    @Override
    public CellRangeAddress nextAddress(CellRangeAddressBase address) {
      Preconditions.checkNotNull(address);
      return new CellRangeAddress(address.getLastRow() + DEFAULT_DISTANCE,
          address.getLastRow() + DEFAULT_DISTANCE, address.getFirstColumn(),
          address.getLastColumn());
    }
  },
  LEFT {
    @Override
    public Direction getNegativeDirection() {
      return RIGHT;
    }

    @Override
    public CellRangeAddress nextAddress(CellRangeAddressBase address) {
      Preconditions.checkNotNull(address);
      return new CellRangeAddress(address.getFirstRow(), address.getLastRow(),
          address.getFirstColumn() - DEFAULT_DISTANCE, address.getFirstColumn() - DEFAULT_DISTANCE);
    }
  },
  RIGHT {
    @Override
    public Direction getNegativeDirection() {
      return LEFT;
    }

    @Override
    public CellRangeAddress nextAddress(CellRangeAddressBase address) {
      Preconditions.checkNotNull(address);
      return new CellRangeAddress(address.getFirstRow(), address.getLastRow(),
          address.getLastColumn() + DEFAULT_DISTANCE, address.getLastColumn() + DEFAULT_DISTANCE);
    }
  };
  private static final int DEFAULT_DISTANCE = 1;

  public abstract Direction getNegativeDirection();

  public abstract CellRangeAddress nextAddress(CellRangeAddressBase address);

  public List<StandardCell> next(StandardCell original) {
    Preconditions.checkNotNull(original);
    CellRangeAddress address = nextAddress(original);

    List<StandardCell> cells = Lists.newLinkedList();
    for (int rowIndex = address.getFirstRow(); rowIndex <= address.getLastRow(); rowIndex++) {
      for (int columnIndex = address.getFirstColumn(); columnIndex <= address
          .getLastColumn(); columnIndex++) {
        StandardCell cell = StandardCell
            .valueOf(ExcelUtil.getCellByIndex(original.getSheet(), rowIndex, columnIndex));
        if (!cells.contains(cell)) {
          cells.add(cell);
        }
      }
    }
    return cells;
  }

  public List<StandardCell> get(StandardCell cell, int distance) {
    Preconditions.checkNotNull(cell);
    Preconditions.checkArgument(distance > 0);

    List<StandardCell> cells = Lists.newArrayList(cell);
    for (int i = 0; i < distance; i++) {
      cells = cells.stream().map(this::next).flatMap(Collection::stream)
          .collect(Collectors.toCollection(LinkedList::new));
    }
    return cells;
  }

  public StandardCell nextCell(StandardCell cell) {
    Preconditions.checkNotNull(cell);

    List<StandardCell> cells = next(cell);
    Preconditions.checkState(CollectionUtils.isEmpty(cells));
    return Iterables.getOnlyElement(cells);
  }

  public StandardCell getCell(StandardCell cell, int distance) {
    Preconditions.checkNotNull(cell);
    Preconditions.checkArgument(distance > 0);

    List<StandardCell> cells = get(cell, distance);
    Preconditions.checkState(CollectionUtils.isEmpty(cells));
    return Iterables.getOnlyElement(cells);
  }
}
