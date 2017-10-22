package com.caotc.excel4j.constant;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.CellRangeAddressBase;
import com.caotc.excel4j.parse.result.StandardCell;
import com.caotc.excel4j.util.ExcelUtil;
import com.google.common.base.Optional;
import com.google.common.base.Preconditions;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

public enum Direction {
  TOP {
    @Override
    public boolean isBorderCell(StandardCell cell) {
      return cell.containsRow(cell.getSheet().getFirstRowNum());
    }

    @Override
    public Direction getNegativeDirection() {
      return BOTTOM;
    }

    @Override
    public CellRangeAddress getAddress(CellRangeAddressBase address, int distance) {
      Preconditions.checkNotNull(address);
      return new CellRangeAddress(address.getFirstRow() - distance,
          address.getFirstRow() - distance, address.getFirstColumn(), address.getLastColumn());
    }
  },
  BOTTOM {
    @Override
    public boolean isBorderCell(StandardCell cell) {
      return cell.containsRow(cell.getSheet().getLastRowNum());
    }

    @Override
    public Direction getNegativeDirection() {
      return TOP;
    }

    @Override
    public CellRangeAddress getAddress(CellRangeAddressBase address, int distance) {
      Preconditions.checkNotNull(address);
      return new CellRangeAddress(address.getLastRow() + distance, address.getLastRow() + distance,
          address.getFirstColumn(), address.getLastColumn());
    }
  },
  LEFT {
    @Override
    public boolean isBorderCell(StandardCell cell) {
      return Iterables.any(cell.getRows(), row -> cell.containsColumn(row.getFirstCellNum()));
    }

    @Override
    public Direction getNegativeDirection() {
      return RIGHT;
    }

    @Override
    public CellRangeAddress getAddress(CellRangeAddressBase address, int distance) {
      Preconditions.checkNotNull(address);
      return new CellRangeAddress(address.getFirstRow(), address.getLastRow(),
          address.getFirstColumn() - distance, address.getFirstColumn() - distance);
    }
  },
  RIGHT {
    @Override
    public boolean isBorderCell(StandardCell cell) {
      return Iterables.any(cell.getRows(), row -> cell.containsColumn(row.getLastCellNum()));
    }

    @Override
    public Direction getNegativeDirection() {
      return LEFT;
    }

    @Override
    public CellRangeAddress getAddress(CellRangeAddressBase address, int distance) {
      Preconditions.checkNotNull(address);
      return new CellRangeAddress(address.getFirstRow(), address.getLastRow(),
          address.getLastColumn() + distance, address.getLastColumn() + distance);
    }
  };
  private static final int DEFAULT_DISTANCE = 1;

  public abstract boolean isBorderCell(StandardCell cell);

  public abstract Direction getNegativeDirection();

  public abstract CellRangeAddress getAddress(CellRangeAddressBase address, int distance);

  public CellRangeAddress nextAddress(CellRangeAddressBase address) {
    return getAddress(address, DEFAULT_DISTANCE);
  }

  public List<StandardCell> next(StandardCell original) {
    Preconditions.checkNotNull(original);

    if (isBorderCell(original)) {
      return Collections.emptyList();
    }

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

  public Optional<StandardCell> nextCell(StandardCell cell) {
    Preconditions.checkNotNull(cell);
    List<StandardCell> cells = next(cell);
    return CollectionUtils.isEmpty(cells) ? Optional.absent()
        : Optional.of(Iterables.getOnlyElement(cells));
  }

  public Optional<StandardCell> getCell(StandardCell cell, int distance) {
    Preconditions.checkNotNull(cell);
    Preconditions.checkArgument(distance > 0);

    List<StandardCell> cells = get(cell, distance);
    return CollectionUtils.isEmpty(cells) ? Optional.absent()
        : Optional.of(Iterables.getOnlyElement(cells));
  }
}
