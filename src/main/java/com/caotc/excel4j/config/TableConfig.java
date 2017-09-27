package com.caotc.excel4j.config;

import com.caotc.excel4j.constant.Direction;

public class TableConfig {
  private Direction fixedMenuDirection;
  private Direction unFixedMenuDirection;
  public Direction getFixedMenuDirection() {
    return fixedMenuDirection;
  }
  public void setFixedMenuDirection(Direction fixedMenuDirection) {
    this.fixedMenuDirection = fixedMenuDirection;
  }
  public Direction getUnFixedMenuDirection() {
    return unFixedMenuDirection;
  }
  public void setUnFixedMenuDirection(Direction unFixedMenuDirection) {
    this.unFixedMenuDirection = unFixedMenuDirection;
  }
  
}
