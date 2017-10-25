package com.caotc.excel4j.parse.result;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Map;
import com.caotc.excel4j.config.DataConfig;
import com.google.common.base.Optional;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import com.google.common.reflect.TypeToken;

public class Data<T> {
  private final Menu menu;
  private final DataConfig dataConfig;
  private final ImmutableList<Object> values;

  public Data(Menu menu, DataConfig dataConfig, ImmutableList<Object> values) {
    super();
    this.menu = menu;
    this.dataConfig = dataConfig;
    this.values = values;
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
  public void toJavaObject() {
    Optional<Menu> optional = menu.getFieldParent();
    Class<?> parentType = optional.isPresent() ? optional.get().getField().get().getType()
        : dataConfig.getMenuConfig().getTableConfig().getType();
    TypeToken<?> parentTypeToken = TypeToken.of(parentType);

    Field field = dataConfig.getField();
    Class<?> type = field.getType();
    TypeToken<?> typeToken = TypeToken.of(type);
    boolean typeSingle = !(typeToken.isArray() || typeToken.isSubtypeOf(Collection.class)
        || typeToken.isSubtypeOf(Map.class));
    // TODO
    if (parentTypeToken.isArray() || parentTypeToken.isSubtypeOf(Collection.class)
        || parentTypeToken.isSubtypeOf(Map.class)) {
      if (typeSingle) {
        
      } else {

      }
    } else {
      if (typeSingle) {
        menu.cast(Iterables.getOnlyElement(values, null), typeToken.getRawType());
      } else {

      }
    }

  }

  public Menu getMenu() {
    return menu;
  }

  public DataConfig getDataConfig() {
    return dataConfig;
  }

  public ImmutableList<Object> getValueCells() {
    return values;
  }

}
