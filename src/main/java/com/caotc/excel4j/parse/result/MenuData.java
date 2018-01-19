package com.caotc.excel4j.parse.result;

import java.util.Optional;
import com.caotc.excel4j.config.MenuDataConfig;
import com.google.common.collect.ImmutableList;
import com.caotc.excel4j.parse.error.ConstraintViolation;

public class MenuData {
  private final Menu menu;
  private final MenuDataConfig config;
  private final ImmutableList<ConstraintViolation<MenuData>> errors;
  private final ImmutableList<StandardCell> valueCells;

  public MenuData(Menu menu) {
    super();
    this.menu = menu;
    this.config = menu.getConfig().getDataConfig();
    this.valueCells = menu.getChildrens().isEmpty() ? config.getLoadType().getDataCells(menu)
        : ImmutableList.of();

    // TODO is DataError?
    errors = valueCells.stream().map(config.getMatcher()::match)
        .filter(Optional::isPresent).map(Optional::get)
        .map(message -> new ConstraintViolation<MenuData>(this, message)).collect(ImmutableList.toImmutableList());
  }

  // TODO
  // public JSONObject toJson() {
  // JSONObject result = new JSONObject();
  // menuToCells.asMap().forEach((key, values) -> {
  // ImmutableList<String> FieldNames = key.getFields();
  // JSONObject object=result;
  // for (int i = 0; i < FieldNames.size(); i++) {
  //
  // }
  // });
  // return result;
  // }

  // public Optional getValue() {
  // V value = null;
  // // TODO
  // if (Objects.nonNull(dataConfig.getFieldName())) {
  // // TODO DataMenu时是否有返回值
  // // if (menu.isDataMenu()) {
  // // if (Objects.equals(ConstructType.OBJECT, dataConfig.getCastType()) &&
  // // menu.isFixedDataMenu()
  // // && dataConfig.getDataNumber() == 1) {
  // // value=dataConfig.cast(Iterables.getOnlyElement(valueCells).getValue());
  // // }
  // // value=dataConfig.cast(getCellValues());
  // // }else {
  // // value=dataConfig.getCastType().constructValue(menu);
  // // }
  // if (!menu.isDataMenu()) {
  // ImmutableList<Menu<?>> childrens = menu.getFieldChildrens();
  // Map<String, Object> vars = Maps.newHashMap();
  // // TODO 优化
  // childrens.stream().filter(Menu::isDataMenu).forEach(m -> {
  // vars.put((String) m.getFieldName().get(), menu.getData().getCellValues());
  // });
  // childrens.stream().filter(m -> !m.isDataMenu()).forEach(m -> {
  // vars.put((String) m.getFieldName().get(), menu.getData().getValue());
  // });
  // value = dataConfig.getConstructType().construct(dataConfig.getType(), vars,
  // menu.getMenuConfig().getEffectiveParserConfig());
  // }
  // }
  // return Optional.ofNullable(value);
  // }

//  public ImmutableList getCellValues() {
//    // TODO
//    return valueCells.stream().map(StandardCell::getValue).map(dataConfig::cast)
//        .collect(ImmutableList.toImmutableList());
//  }

  public Menu getMenu() {
    return menu;
  }

  public MenuDataConfig getConfig() {
    return config;
  }

  public ImmutableList<StandardCell> getValueCells() {
    return valueCells;
  }

  public ImmutableList<ConstraintViolation<MenuData>> getErrors() {
    return errors;
  }

}
