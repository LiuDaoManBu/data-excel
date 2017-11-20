package com.caotc.excel4j.parse.result;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import com.caotc.excel4j.config.DataConfig;
import com.caotc.excel4j.constant.ConstructType;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Iterables;
import com.google.common.reflect.TypeToken;

public class Data<T> {
  private final Menu<T> menu;
  private final DataConfig<T> dataConfig;
  private final ImmutableList<StandardCell> valueCells;

  public Data(Menu<T> menu, DataConfig<T> dataConfig, ImmutableList<StandardCell> valueCells) {
    super();
    this.menu = menu;
    this.dataConfig = dataConfig;
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

  public Optional<T> getValue() {
    //TODO
    if (Objects.isNull(dataConfig.getFieldName())) {
      return Optional.empty();
    }
    if (menu.isDataMenu()) {
      if (Objects.equals(ConstructType.OBJECT, dataConfig.getCastType()) && menu.isFixedDataMenu()
          && dataConfig.getDataNumber() == 1) {
        return Optional
            .ofNullable(dataConfig.cast(Iterables.getOnlyElement(valueCells).getValue()));
      }
      return Optional.ofNullable(dataConfig.cast(getCellValues()));
    }
    return Optional.ofNullable(dataConfig.getCastType().constructValue(menu));
  }

  public ImmutableList<T> getCellValues() {
    // TODO
    return FluentIterable.from(valueCells).transform(StandardCell::getValue)
        .transform(dataConfig::cast).toList();
  }

  // TODO
  public <T> void setFieldValue(T Object) {
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

  public Menu<T> getMenu() {
    return menu;
  }

  public DataConfig<T> getDataConfig() {
    return dataConfig;
  }

  public ImmutableList<StandardCell> getValueCells() {
    return valueCells;
  }


}
