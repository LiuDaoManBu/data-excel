package com.caotc.excel4j.parse.result;

import java.lang.reflect.Field;
import com.caotc.excel4j.config.DataConfig;
import com.google.common.collect.ImmutableList;

public class Data {
  private final Menu menu;
  private final DataConfig dataConfig;
  private final ImmutableList<StandardCell> valueCells;

  public Data(Menu menu,DataConfig dataConfig, ImmutableList<StandardCell> valueCells) {
    super();
    this.menu = menu;
    this.dataConfig=dataConfig;
    this.valueCells = valueCells;
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

  // TODO
  public <T> T toJavaObject() {
    Field field=dataConfig.getField();
    Class<?> type=field.getType();
    return null;
  }

  public Menu getMenu() {
    return menu;
  }

  public DataConfig getDataConfig() {
    return dataConfig;
  }

  public ImmutableList<StandardCell> getValueCells() {
    return valueCells;
  }

}
