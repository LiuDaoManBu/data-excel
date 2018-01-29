package com.github.liudaomanbu.excel.parse.result;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import com.github.liudaomanbu.excel.config.TableDataConfig;
import com.github.liudaomanbu.excel.matcher.data.type.BaseDataType;
import com.github.liudaomanbu.excel.parse.error.ValidationError;
import com.github.liudaomanbu.excel.util.ExcelUtil;
import com.github.liudaomanbu.excel.validator.BaseValidator;
import com.github.liudaomanbu.excel.validator.Validator;
import com.google.common.base.Joiner;
import com.google.common.collect.ImmutableBiMap;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

public class TableData<T> {
  private static final Joiner JOINER = Joiner.on("").skipNulls();
  private static final ImmutableBiMap<BaseDataType, String> DATA_TYPE_TO_TIPS =
      ImmutableBiMap.<BaseDataType, String>builder().put(BaseDataType.BOOLEAN, "是否")
          .put(BaseDataType.CHINESE, "中文").put(BaseDataType.DATE, "日期")
          .put(BaseDataType.DATE_TIME, "日期时间").put(BaseDataType.DECIMAL, "小数")
          .put(BaseDataType.EMAIL, "邮箱").put(BaseDataType.ENGLISH, "英语")
          .put(BaseDataType.ENGLISH_OR_NUMBER, "英语或数字").put(BaseDataType.ENUM, "枚举")
          .put(BaseDataType.ID_CARD_NUMBER, "身份证号码").put(BaseDataType.NATURAL_NUMBER, "自然数")
          .put(BaseDataType.NEGATIVE_DECIMAL, "负小数").put(BaseDataType.NEGATIVE_NUMBER, "负数")
          .put(BaseDataType.NEGATIVE_WHOLE_NUMBER, "负整数").put(BaseDataType.NUMBER, "数字")
          .put(BaseDataType.PHONE, "电话号码").put(BaseDataType.POSITIVE_DECIMAL, "正小数")
          .put(BaseDataType.POSITIVE_NUMBER, "正数").put(BaseDataType.POSITIVE_WHOLE_NUMBER, "正整数")
          .put(BaseDataType.STRING, "字符串").put(BaseDataType.TELEPHONE, "手机号码")
          .put(BaseDataType.TIME, "时间").put(BaseDataType.WHOLE_NUMBER, "整数").build();

  private final Table<T> table;
  private final TableDataConfig<T> config;
  private final ImmutableList<Data<T>> datas;
  private final ImmutableList<ValidationError<TableData<T>>> errors;

  public TableData(Table<T> table) {
    this.table = table;
    this.config = table.getConfig().getDataConfig();
    List<Map<Menu<T>, StandardCell>> menuTodatas = Lists.newArrayList();
    ImmutableList<Menu<T>> menus = table.getDataMenus().collect(ImmutableList.toImmutableList());

    ImmutableMap<Menu<T>, StandardCell> menuToFixedDatas =
        menus.stream().filter(Menu::isSingleDataMenu).collect(ImmutableMap.toImmutableMap(
            Function.identity(), menu -> Iterables.getOnlyElement(menu.getData().getValueCells())));
    menuTodatas.add(menuToFixedDatas);

    menus.stream().filter(Menu::isUnFixedDataMenu).forEach(menu -> {
      ImmutableList<StandardCell> valueCells = menu.getData().getValueCells();
      for (int j = 0; j < valueCells.size(); j++) {
        StandardCell valueCell = valueCells.get(j);

        Map<Menu<T>, StandardCell> map = null;
        if (j < menuTodatas.size()) {
          map = menuTodatas.get(j);
        } else {
          map = Maps.newHashMap(menuToFixedDatas);
          menuTodatas.add(map);
        }
        map.put(menu, valueCell);
      }
    });

    datas = menuTodatas.stream().map(map -> new Data<>(map, config.getType(),
        config.getBeforeTransform(), config.getBeforeValidator()))
        .collect(ImmutableList.toImmutableList());

    this.errors = getValidators()
        .flatMap(validator -> datas.stream().filter(validator::premise).map(validator::validate)
            .flatMap(Collection::stream))
        .map(error -> new ValidationError<>(this, error.getMessage()))
        .collect(ImmutableList.toImmutableList());
  }

  private Stream<Validator<Data<T>>> getValidators() {
    return Stream.concat(createDataValidator(), Optional.ofNullable(config)
        .map(TableDataConfig::getValidators).map(Collection::stream).orElseGet(Stream::empty));
  }

  private Stream<Validator<Data<T>>> createDataValidator() {
    return table.getDataMenus()
        .map(menu -> new BaseValidator<Data<T>>(
            data -> menu.getData().getConfig().getDataType()
                .test(data.getMenuToValueCells().get(menu).getValue()),
            data -> JOINER.join(menu.getFullName(),
                data.getMenuToValueCells().get(menu).formatAsString(), "单元格",
                data.getMenuToValueCells().get(menu).getValue(), "不符合",
                DATA_TYPE_TO_TIPS.get(menu.getData().getConfig().getDataType()), "格式")));
  }

  public Table<T> getTable() {
    return table;
  }

  public TableDataConfig<T> getConfig() {
    return config;
  }


  public ImmutableList<ValidationError<TableData<T>>> getErrors() {
    return errors;
  }

  public ImmutableList<Data<T>> getDatas() {
    return datas;
  }

  public ImmutableList<Data<T>> getEffectiveDatas() {
    return getDatas().stream().filter(data -> !data.getJson().values().isEmpty())
        .collect(ImmutableList.toImmutableList());
  }

  public ImmutableList<T> getEffectiveValues() {
    return getEffectiveDatas().stream()
        .map(t -> (T) ExcelUtil.toJavaObject(t.getMenuToValueCells(), config.getType()))
        .collect(ImmutableList.toImmutableList());
  }

  public List<Map<String, Object>> getEffectiveJsons() {
    return getEffectiveDatas().stream().map(Data::getMenuToValueCells).map(ExcelUtil::toJsonObject)
        .collect(Collectors.toList());
  }
}
