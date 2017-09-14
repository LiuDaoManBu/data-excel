package com.caotc.excel4j.parse.result;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Cell;
import com.alibaba.fastjson.JSONObject;
import com.caotc.excel4j.util.ClassUtils;
import com.caotc.excel4j.util.ExcelUtil;

public class Data {
  private SheetParseResult sheetParseResult;
  private final Collection<CellData> cellDatas;
  private final JSONObject jsonData = new JSONObject();

  public Data(Collection<CellData> cellDatas) {
    super();
    this.cellDatas = cellDatas;
    for (CellData cellData : cellDatas) {
      Menu menu = cellData.getMenu();
      Object dataValue = cellData.getValue();
      if (dataValue != null) {
        jsonData.put(menu.getName(), dataValue);
        if (menu.getMenuConfig() != null) {
          if (StringUtils.isNotBlank(menu.getMenuConfig().getFieldName())) {
            jsonData.put(menu.getMenuConfig().getFieldName(), dataValue);
          }
          // if(StringUtils.isNotBlank(menu.getMatchName())){
          // jsonData.put(menu.getMatchName(), dataValue);
          // }
        }
      }

    }
  }

  public Collection<CellData> getMenuToCells() {
    return cellDatas;
  }

  public JSONObject getJsonData() {
    return jsonData;
  }

  public <T> T toJavaObject(Class<T> type) {
    Map<String, Field> nameToFields = ClassUtils.getNameToFields(type);
    cellDatas.forEach((cellData) -> {
      String key = cellData.getMenu().getCheckMenuConfig().getFieldName();
      if (nameToFields.containsKey(key)) {
        jsonData.put(key, cellData.getValue(nameToFields.get(key).getType()));
      }
    });
    return jsonData.toJavaObject(type);
  }
}
