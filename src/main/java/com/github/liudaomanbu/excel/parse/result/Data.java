package com.github.liudaomanbu.excel.parse.result;

import java.util.Map;
import java.util.function.Consumer;
import com.alibaba.fastjson.JSONObject;
import com.github.liudaomanbu.excel.util.ExcelUtil;

public class Data<T> {
  private final Map<Menu<T>, StandardCell> menuToValueCells;
  private final Consumer<Map<String, Object>> beforeTransform;
  private final Map<String, Object> json;
  private final Consumer<T> beforeValidator;
  private final T value;

  public Data(Map<Menu<T>, StandardCell> menuToValueCells, Class<T> type,
      Consumer<Map<String, Object>> beforeTransform, Consumer<T> beforeValidator) {
    this.menuToValueCells = menuToValueCells;
    this.beforeTransform = beforeTransform;
    json = ExcelUtil.toJsonObject(menuToValueCells);
    beforeTransform.accept(json);
    this.beforeValidator = beforeValidator;
    value = new JSONObject(json).toJavaObject(type);
    beforeValidator.accept(value);
  }

  public Menu<T> getMenuByFieldName(String fieldName) {
    return menuToValueCells.keySet().stream().filter(menu -> menu.getFieldName().equals(fieldName))
        .findAny().orElse(null);
  }

  public Map<Menu<T>, StandardCell> getMenuToValueCells() {
    return menuToValueCells;
  }

  public Map<String, Object> getJson() {
    return json;
  }

  public T getValue() {
    return value;
  }
}
