package com.caotc.excel4j.parse.result;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;

public class Data {
  private final Table table;
  private final ListMultimap<Menu,StandardCell> menuToCells;
  
  public Data(Table table, ListMultimap<Menu, StandardCell> menuToCells) {
    super();
    this.table = table;
    this.menuToCells = menuToCells;
  }
  
  
  //TODO
//  public <T> T toJavaObject(Class<T> type) {
//    Map<String, Field> nameToFields = ClassUtils.getNameToFields(type);
//    cellDatas.forEach((cellData) -> {
//      String key = cellData.getMenu().getCheckMenuConfig().getFieldName();
//      if (nameToFields.containsKey(key)) {
//        jsonData.put(key, cellData.getValue(nameToFields.get(key).getType()));
//      }
//    });
//    return jsonData.toJavaObject(type);
//  }
}
