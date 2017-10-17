package com.caotc.excel4j.parse.result;

import java.util.List;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.constant.Direction;
import com.google.common.base.Preconditions;
import com.google.common.collect.Lists;

public class Menu {
  public static class Builder {
    private StandardCell cell;
    private MenuConfig menuConfig;
    private Table table;
    private Menu parentMenu;
    private List<Menu> childrenMenus = Lists.newArrayList();
    
    public Menu build() {
      Preconditions.checkNotNull(cell);
      Preconditions.checkArgument(table != null || parentMenu != null);
      Preconditions.checkArgument(menuConfig != null || parentMenu != null);

      if (parentMenu != null && table == null) {
        table = parentMenu.table;
      }
      return new Menu(this);
    }

    public StandardCell getCell() {
      return cell;
    }

    public Builder setCell(StandardCell cell) {
      this.cell = cell;
      return this;
    }

    public MenuConfig getMenuConfig() {
      return menuConfig;
    }

    public Builder setMenuConfig(MenuConfig menuConfig) {
      this.menuConfig = menuConfig;
      return this;
    }

    public Table getTable() {
      return table;
    }

    public Builder setTable(Table table) {
      this.table = table;
      return this;
    }

    public Menu getParentMenu() {
      return parentMenu;
    }

    public Builder setParentMenu(Menu parentMenu) {
      this.parentMenu = parentMenu;
      return this;
    }

    public List<Menu> getChildrenMenus() {
      return childrenMenus;
    }

    public Builder setChildrenMenus(List<Menu> childrenMenus) {
      this.childrenMenus = childrenMenus;
      return this;
    }
    
  }

  public static Builder builder() {
    return new Builder();
  }

  private final StandardCell cell;
  private final MenuConfig menuConfig;
  private final Table table;
  private final Menu parentMenu;
  private final List<Menu> childrenMenus;


  public Menu(Builder builder) {
    cell = builder.cell;
    menuConfig = builder.menuConfig;
    table = builder.table;
    parentMenu = builder.parentMenu;
    childrenMenus = builder.childrenMenus;
  }

  public void checkDataCell(StandardCell dataCell) {
    MenuConfig config = getCheckMenuConfig();
    if (config.getDataMatcher() != null) {
      // TODO
      Object value = dataCell.getValue();
      // if(!checkMenuConfig.getDataMatcher().matches(value)){
      // StringBuffer errorMessage=new StringBuffer();
      // errorMessage.append("工作簿").append(dataCell.getSheet().getSheetName()).append("第").append(dataCell
      // .getRowIndex()+1).append("行").append(ExcelUtil.indexToColumn(dataCell.getColumnIndex()+1))
      // .append("列的").append(getName()).append("数据格式不正确");
      // addError(errorMessage.toString());
      // }

      // if(StringUtils.isEmpty(cellString)){
      // if(checkMenuConfig.isCheckEmpty()){
      // errorMessage.append("不能为空");
      // addError(errorMessage.toString());
      // }
      // }else{
      // Integer maxLength=checkMenuConfig.getMaxLength();
      // if(maxLength!=null && maxLength>=0 && cellString.length()>maxLength){
      // errorMessage.append("长度超过").append(checkMenuConfig.getMaxLength());
      // addError(errorMessage.toString());
      // }
      //
      // String regex=checkMenuConfig.getRegex();
      // if(regex!=null){
      // regex+=checkMenuConfig.isCheckEmpty()?"+":"*";
      // if(!cellString.matches(regex)){
      // String regexTip=ExcelUtil.REGEX_AND_TIP_MAP.get(checkMenuConfig.getRegex());
      // if(regexTip!=null){
      // errorMessage.append("数据格式不正确，应为").append(regexTip);
      // }else{
      // errorMessage.append("数据格式不正确");
      // }
      // addError(errorMessage.toString());
      // }
      // }
      // }
    }
  }

  public StandardCell getDataCell(int serialNumber) {
    StandardCell dataCell = cell;
    for (int i = 1; i <= serialNumber; i++) {
      dataCell = nextDataCell(dataCell);
    }
    return dataCell;
  }

  public StandardCell nextDataCell(StandardCell cell) {
    MenuConfig config = getCheckMenuConfig();
    Direction direction = config.getDirection();
    if (cell == null || this.cell.equals(cell)) {
      return direction.getCell(this.cell, config.getDistance());
    }
    return direction.nextCell(cell);
  }

  public MenuConfig getCheckMenuConfig() {
    return menuConfig == null
        ? getParentMenu() == null ? null : getParentMenu().getCheckMenuConfig()
        : menuConfig;
  }

  public void load() {
    getCheckMenuConfig().load(this);
  }

  public void addChildrenMenu(Menu childrenMenu) {
    childrenMenus.add(childrenMenu);
  }

  public boolean hasChildrenMenu(Menu childrenMenu) {
    return childrenMenus.contains(childrenMenu);
  }

  public boolean hasChildrenMenu(StandardCell cell) {
    return childrenMenus.stream().anyMatch(childrenMenu -> childrenMenu.getCell().equals(cell));
  }

  public String getName() {
    return cell.getValueCell().getStringCellValue();
  }

  public boolean isDataMenu() {
    return getCheckMenuConfig().isDataMenu();
  }

  public boolean isFixedDataMenu() {
    return getCheckMenuConfig().isFixedDataMenu();
  }

  public boolean isUnFixedDataMenu() {
    return getCheckMenuConfig().isUnFixedDataMenu();
  }

  public boolean isMixedDataMenu() {
    return getCheckMenuConfig().isMixedDataMenu();
  }

  public boolean isMustMenu() {
    return getCheckMenuConfig().isMustMenu();
  }

  public boolean isNotMustMenu() {
    return getCheckMenuConfig().isNotMustMenu();
  }

  public StandardCell getCell() {
    return cell;
  }

  public MenuConfig getMenuConfig() {
    return menuConfig;
  }

  public Menu getParentMenu() {
    return parentMenu;
  }

  public Table getTable() {
    return table;
  }

  public List<Menu> getChildrenMenus() {
    return childrenMenus;
  }
}
