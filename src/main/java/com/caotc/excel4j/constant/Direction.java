package com.caotc.excel4j.constant;

import java.util.Collection;
import java.util.Optional;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.CellRangeAddressBase;
import com.caotc.excel4j.parse.result.StandardCell;
import com.caotc.excel4j.util.ExcelUtil;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;

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
      return ExcelUtil.getRows(cell.getSheet(), cell.getFirstRow(), cell.getLastRow())
          .anyMatch(row -> cell.containsColumn(row.getFirstCellNum()));
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
      return ExcelUtil.getRows(cell.getSheet(), cell.getFirstRow(), cell.getLastRow())
          .anyMatch(row -> cell.containsColumn(row.getLastCellNum()));
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

  public ImmutableList<StandardCell> next(StandardCell original) {
    Preconditions.checkNotNull(original);

    if (isBorderCell(original)) {
      return ImmutableList.of();
    }

    CellRangeAddress address = nextAddress(original);

    ImmutableList.Builder<StandardCell> cells = ImmutableList.builder();
    for (int rowIndex = address.getFirstRow(); rowIndex <= address.getLastRow(); rowIndex++) {
      for (int columnIndex = address.getFirstColumn(); columnIndex <= address
          .getLastColumn(); columnIndex++) {
        Cell cell = ExcelUtil.getCellByIndex(original.getSheet(), rowIndex, columnIndex);
        StandardCell standardCell = StandardCell.valueOf(cell);
        if (standardCell.getValueCell().equals(cell)) {
          cells.add(standardCell);
        }
      }
    }
    return cells.build();
  }

  public ImmutableList<StandardCell> get(StandardCell cell, int distance) {// TODO distance名称不准确
    Preconditions.checkNotNull(cell);
    Preconditions.checkArgument(distance > 0);

    ImmutableList<StandardCell> cells = ImmutableList.of(cell);
    for (int i = 0; i < distance; i++) {
      cells = cells.stream().map(this::next).flatMap(Collection::stream)
          .collect(ImmutableList.toImmutableList());
    }
    return cells;
  }

  public Optional<StandardCell> nextCell(StandardCell cell) {
    Preconditions.checkNotNull(cell);
    return Optional.of(next(cell)).filter(t -> !t.isEmpty()).map(Iterables::getOnlyElement);
  }

  public Optional<StandardCell> getCell(StandardCell cell, int distance) {
    Preconditions.checkNotNull(cell);
    Preconditions.checkArgument(distance > 0);

    return Optional.of(next(cell)).filter(t -> !t.isEmpty()).map(Iterables::getOnlyElement);
  }
}
