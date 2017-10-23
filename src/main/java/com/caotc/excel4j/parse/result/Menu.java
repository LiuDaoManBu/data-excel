package com.caotc.excel4j.parse.result;

import java.util.Collection;
import java.util.List;
import org.apache.commons.collections4.CollectionUtils;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.constant.Direction;
import com.google.common.base.Optional;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;

public class Menu {
  public static class Builder {
    private StandardCell cell;
    private MenuConfig menuConfig;
    private Table table;
    private Menu parentMenu;

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

  }

  public static Builder builder() {
    return new Builder();
  }

  private final StandardCell cell;
  private final MenuConfig menuConfig;
  private final Table table;
  private final Menu parentMenu;
  private final ImmutableList<Menu> childrenMenus;


  public Menu(Builder builder) {
    cell = builder.cell;
    menuConfig = builder.menuConfig;
    table = builder.table;
    parentMenu = builder.parentMenu;

    childrenMenus = menuConfig.getDataConfig().getLoadType().getChildrenMenus(this);
  }

  private ImmutableList<Menu> loadChildrenMenus(){
    com.google.common.collect.ImmutableList.Builder<Menu> builder = ImmutableList.builder();

    ImmutableCollection<MenuConfig> childrenConfigs = menuConfig.getChildrenMenuConfigs();
    if (!CollectionUtils.isEmpty(childrenConfigs)) {
      List<StandardCell> menuCells =
          menuConfig.getDirection().get(getCell(), menuConfig.getDistance());
      if (!CollectionUtils.isEmpty(menuCells)) {
        childrenConfigs.forEach(childrenConfig -> {
          Optional<StandardCell> optional = Iterables.tryFind(menuCells, childrenConfig::matches);
          if (optional.isPresent()) {
            builder.add(Menu.builder().setCell(optional.get()).setMenuConfig(childrenConfig)
                .setParentMenu(this).build());
          }
        });
      }
    }
    return builder.build();
  }
  
  public void checkDataCell(StandardCell dataCell) {
//    if (menuConfig.getDataMatcher() != null) {
      // TODO
//      Object value = dataCell.getValue();
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
//    }
  }

  public Optional<StandardCell> nextDataCell(StandardCell cell) {
    if (cell == null) {
      cell = this.cell;
    }

    Direction direction = menuConfig.getDirection();

    return this.cell.equals(cell) ? direction.getCell(cell, menuConfig.getDistance())
        : direction.nextCell(cell);
  }

  public boolean hasChildrenMenu(Menu childrenMenu) {
    return childrenMenus.contains(childrenMenu);
  }

  public boolean hasChildrenMenu(StandardCell cell) {
    return Iterables.any(childrenMenus, childrenMenu -> childrenMenu.getCell().equals(cell));
  }

  public String getName() {
    return cell.getValueCell().getStringCellValue();
  }

  public Optional<String> getFieldName() {
    return Optional.fromNullable(menuConfig == null?getName():menuConfig.getFieldName());
  }

  public boolean isDataMenu() {
    return menuConfig.isDataMenu();
  }
  
  // delegate methods start
  public boolean isFixedDataMenu() {
    return menuConfig.isFixedDataMenu();
  }

  public boolean isUnFixedDataMenu() {
    return menuConfig.isUnFixedDataMenu();
  }

  public boolean isMixedDataMenu() {
    return menuConfig.isMixedDataMenu();
  }

  public boolean isMustMenu() {
    return menuConfig.isMustMenu();
  }

  public boolean isNotMustMenu() {
    return menuConfig.isNotMustMenu();
  }

  public boolean matches(Object value) {
    return menuConfig.matches(value);
  }

  public boolean support(Object value) {
    return menuConfig.support(value);
  }

  public Collection<Class<?>> canCastClasses() {
    return menuConfig.canCastClasses();
  }

  public <T> boolean canCast(Class<T> clazz) {
    return menuConfig.canCast(clazz);
  }

  public <T> T cast(Object value, Class<T> clazz) {
    return menuConfig.cast(value, clazz);
  }
  // delegate methods end

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

  public ImmutableList<Menu> getChildrenMenus() {
    return childrenMenus;
  }
}
