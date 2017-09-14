package com.caotc.excel4j.constant;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.poi.ss.util.CellRangeAddress;
import com.caotc.excel4j.parse.result.StandardCell;
import com.caotc.excel4j.util.ExcelUtil;
import com.google.common.collect.Lists;

public enum Direction {
  TOP {
    @Override
    public Direction getNegativeDirection() {
      return BOTTOM;
    }

    @Override
    protected CellRangeAddress nextCellRangeAddress(StandardCell cell) {
      if (cell == null) {
        return null;
      }
      return new CellRangeAddress(cell.getFirstRow() - DEFAULT_DISTANCE,
          cell.getFirstRow() - DEFAULT_DISTANCE, cell.getFirstColumn(), cell.getLastColumn());
    }
  },
  BOTTOM {
    @Override
    public Direction getNegativeDirection() {
      return TOP;
    }

    @Override
    protected CellRangeAddress nextCellRangeAddress(StandardCell cell) {
      if (cell == null) {
        return null;
      }
      return new CellRangeAddress(cell.getLastRow() + DEFAULT_DISTANCE,
          cell.getLastRow() + DEFAULT_DISTANCE, cell.getFirstColumn(), cell.getLastColumn());
    }
  },
  LEFT {
    @Override
    public Direction getNegativeDirection() {
      return RIGHT;
    }

    @Override
    protected CellRangeAddress nextCellRangeAddress(StandardCell cell) {
      if (cell == null) {
        return null;
      }
      return new CellRangeAddress(cell.getFirstRow(), cell.getLastRow(),
          cell.getFirstColumn() - DEFAULT_DISTANCE, cell.getFirstColumn() - DEFAULT_DISTANCE);
    }
  },
  RIGHT {
    @Override
    public Direction getNegativeDirection() {
      return LEFT;
    }

    @Override
    protected CellRangeAddress nextCellRangeAddress(StandardCell cell) {
      if (cell == null) {
        return null;
      }
      return new CellRangeAddress(cell.getFirstRow(), cell.getLastRow(),
          cell.getLastColumn() + DEFAULT_DISTANCE, cell.getLastColumn() + DEFAULT_DISTANCE);
    }
  };
  private static final int DEFAULT_DISTANCE = 1;

  public abstract Direction getNegativeDirection();

  public List<StandardCell> nextCells(StandardCell cell) {
    CellRangeAddress nextCellRangeAddress = null;
    if (cell == null || (nextCellRangeAddress = nextCellRangeAddress(cell)) == null) {
      return Collections.emptyList();
    }

    List<StandardCell> nextCells = Lists.newLinkedList();
    for (int rowIndex = nextCellRangeAddress.getFirstRow(); rowIndex <= nextCellRangeAddress
        .getLastRow(); rowIndex++) {
      for (int columnIndex =
          nextCellRangeAddress.getFirstColumn(); columnIndex <= nextCellRangeAddress
              .getLastColumn(); columnIndex++) {
        StandardCell nextCell =
            StandardCell.valueOf(ExcelUtil.getCellByIndex(cell.getSheet(), rowIndex, columnIndex));
        if (!nextCells.contains(nextCell)) {
          nextCells.add(nextCell);
        }
      }
    }
    return nextCells;
  }

  public List<StandardCell> getCells(StandardCell cell, int distance) {
    if (cell == null || distance <= 0) {
      return Collections.emptyList();
    }
    List<StandardCell> cells = Lists.newArrayList(cell);
    for (int i = 0; i < distance; i++) {
      cells = cells.stream().map(this::nextCells).flatMap(Collection::stream)
          .collect(Collectors.toCollection(LinkedList::new));
    }
    return cells;
  }

  public StandardCell nextCell(StandardCell cell) {
    if (cell == null) {
      return null;
    }

    List<StandardCell> nextCells = nextCells(cell);
    if (CollectionUtils.isEmpty(nextCells)) {
      return null;
    }

    if (nextCells.size() > 1) {
      throw new IllegalArgumentException(
          "the standardCell" + cell + " in direction" + this + " is more than a next standardCell");
    }
    return nextCells.get(0);
  }

  public StandardCell getCell(StandardCell cell, int distance) {
    if (cell == null || distance <= 0) {
      return null;
    }
    for (int i = 0; i < distance; i++) {
      cell = nextCell(cell);
    }
    return cell;
  }

  protected abstract CellRangeAddress nextCellRangeAddress(StandardCell cell);
}
