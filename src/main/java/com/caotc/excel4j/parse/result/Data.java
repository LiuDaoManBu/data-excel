package com.caotc.excel4j.parse.result;

import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import com.caotc.excel4j.config.DataConfig;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Maps;

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
    T value=null;
    //TODO
    if (Objects.nonNull(dataConfig.getFieldName())) {
      //TODO DataMenu时是否有返回值
//      if (menu.isDataMenu()) {
//        if (Objects.equals(ConstructType.OBJECT, dataConfig.getCastType()) && menu.isFixedDataMenu()
//            && dataConfig.getDataNumber() == 1) {
//          value=dataConfig.cast(Iterables.getOnlyElement(valueCells).getValue());
//        }
//        value=dataConfig.cast(getCellValues());
//      }else {
//        value=dataConfig.getCastType().constructValue(menu);
//      }
      if(!menu.isDataMenu()) {
        FluentIterable<Menu> childrens=menu.getFieldChildrens();
        Map<String,Object> vars=Maps.newHashMap();
        //TODO 优化
        childrens.filter(Menu::isDataMenu).forEach(m->{
          vars.put((String) m.getFieldName().get(), menu.getData().getCellValues());
        });
        childrens.filter(m->!m.isDataMenu()).forEach(m->{
          vars.put((String) m.getFieldName().get(), menu.getData().getValue());
        });
        value=dataConfig.getCastType().construct(dataConfig.getFieldType(), vars);
      }
    }
    return Optional.ofNullable(value);
  }

  public ImmutableList<T> getCellValues() {
    // TODO
    return FluentIterable.from(valueCells).transform(StandardCell::getValue)
        .transform(dataConfig::cast).toList();
  }

  // TODO
  public <T> void setFieldValue(T Object) {

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
