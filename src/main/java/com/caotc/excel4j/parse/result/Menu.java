package com.caotc.excel4j.parse.result;

import java.util.List;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.constant.MenuNecessity;
import com.caotc.excel4j.constant.MenuType;
import com.google.common.collect.Lists;

public class Menu {
  private static class Builder {
    private StandardCell cell;
    private MenuConfig menuConfig;
    private MenuConfig checkMenuConfig;
    private Table table;
    private Menu parentMenu;
    private List<Menu> childrenMenus = Lists.newArrayList();
    private MenuType menuType;

    public Builder cell(StandardCell cell) {
      this.cell = cell;
      return this;
    }

    public Builder menuConfig(MenuConfig menuConfig) {
      this.menuConfig = menuConfig;
      return this;
    }

    public Builder checkMenuConfig(MenuConfig checkMenuConfig) {
      this.checkMenuConfig = checkMenuConfig;
      return this;
    }

    public Builder table(Table table) {
      this.table = table;
      return this;
    }

    public Builder parentMenu(Menu parentMenu) {
      this.parentMenu = parentMenu;
      return this;
    }

    public Builder childrenMenus(List<Menu> childrenMenus) {
      this.childrenMenus = childrenMenus;
      return this;
    }

    public Builder menuType(MenuType menuType) {
      this.menuType = menuType;
      return this;
    }

    public Menu build() {
      return new Menu(this);
    }
  }

  public static Builder builder() {
    return new Builder();
  }

  private final StandardCell cell;
  private final MenuConfig menuConfig;
  private final MenuConfig checkMenuConfig;
  private final Table table;
  private final Menu parentMenu;
  private final List<Menu> childrenMenus;
  private final MenuType menuType;

  public Menu(Builder builder) {
    cell = builder.cell;
    menuConfig = builder.menuConfig;
    checkMenuConfig = builder.checkMenuConfig;
    table = builder.table;
    parentMenu = builder.parentMenu;
    childrenMenus = builder.childrenMenus;
    menuType = builder.menuType;
  }

  public void checkDataCell(StandardCell dataCell) {
    if (hasCheckMenuConfig()) {
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
    Direction direction = checkMenuConfig.getDirection();
    if (cell == null || this.cell.equals(cell)) {
      return direction.getCell(this.cell, checkMenuConfig.getFirstDistance());
    }
    return direction.nextCell(cell);
  }

  public boolean hasCheckMenuConfig() {
    return checkMenuConfig != null;
  }

  public MenuConfig getCheckMenuConfig() {
    return checkMenuConfig;
  }

  public void load() {
    if (MenuType.NO_DATA_MENU.equals(menuType)) {
      getCheckMenuConfig().getLoadType().loadChildren(this);
    }
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
    // TODO
    return cell.getValueCell().getStringCellValue();
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

  public MenuType getMenuType() {
    return menuType;
  }

  public Table getTable() {
    return table;
  }

  public List<Menu> getChildrenMenus() {
    return childrenMenus;
  }

  public boolean isDataMenu() {
    return MenuType.DATA_MENU.equals(getMenuType());
  }

  public boolean isFixedDataMenu() {
    return isDataMenu() && LoadType.FIXED.equals(getCheckMenuConfig().getLoadType());
  }

  public boolean isUnFixedDataMenu() {
    return isDataMenu() && LoadType.UNFIXED.equals(getCheckMenuConfig().getLoadType());
  }

  public boolean isMixedDataMenu() {
    return isDataMenu() && LoadType.MIXED.equals(getCheckMenuConfig().getLoadType());
  }

  public boolean isMustMenu() {
    return MenuNecessity.MUST.equals(getCheckMenuConfig().getMenuNecessity());
  }

  public boolean isNotMustMenu() {
    return MenuNecessity.NOT_MUST.equals(getCheckMenuConfig().getMenuNecessity());
  }
}
