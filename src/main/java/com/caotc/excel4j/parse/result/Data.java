package com.caotc.excel4j.parse.result;

import java.lang.reflect.Field;
import java.util.Map;
import com.alibaba.fastjson.JSONObject;
import com.caotc.excel4j.collect.ImmutableTree;
import com.caotc.excel4j.util.ClassUtils;
import com.google.common.base.Optional;
import com.google.common.collect.Collections2;
import com.google.common.collect.FluentIterable;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.Iterables;

public class Data {
  private final Table table;
  private final ImmutableListMultimap<Menu, StandardCell> menuToCells;

  public Data(Table table, ImmutableListMultimap<Menu, StandardCell> menuToCells) {
    super();
    this.table = table;
    this.menuToCells = menuToCells;
  }

  public ImmutableList<StandardCell> getCells(Menu menu) {
    return menuToCells.get(menu);
  }

  public ImmutableList<StandardCell> getCells(String menuName) {
    return getCells(table.getMenu(menuName).orNull());
  }

  public Optional<StandardCell> getCell(Menu menu) {
    return Optional.fromNullable(Iterables.getOnlyElement(getCells(menu), null));
  }

  public Optional<StandardCell> getCell(String menuName) {
    return getCell(table.getMenu(menuName).orNull());
  }

  public ImmutableList<Object> getValues(Menu menu) {
    return ImmutableList.copyOf(Collections2.transform(getCells(menu), StandardCell::getValue));
  }

  public ImmutableList<Object> getValues(String menuName) {
    return getValues(table.getMenu(menuName).orNull());
  }

  public <T> ImmutableList<T> getValues(Menu menu, Class<T> type) {
    return ImmutableList
        .copyOf(Collections2.transform(getValues(menu), value -> menu.cast(value, type)));
  }

  public <T> ImmutableList<T> getValues(String menuName, Class<T> type) {
    return getValues(table.getMenu(menuName).orNull(), type);
  }

  public Optional<Object> getValue(Menu menu) {
    return getCell(menu).transform(StandardCell::getValue);
  }

  public Optional<Object> getValue(String menuName) {
    return getValue(table.getMenu(menuName).orNull());
  }

  public <T> Optional<T> getValue(Menu menu, Class<T> type) {
    return getValue(menu).transform(value -> menu.cast(value, type));
  }

  public <T> Optional<T> getValue(String menuName, Class<T> type) {
    Menu menu = table.getMenu(menuName).orNull();
    return getValue(menu).transform(value -> menu.cast(value, type));
  }

  public JSONObject toJson() {
    FluentIterable.from(table.getMenuTrees())
        .transform(menuTree -> ImmutableTree.using(menuTree.getRoot(), menu -> {
          FluentIterable<Menu> result = null;
          for (Iterable<Menu> childrens = menu.getChildrenMenus(); !Iterables.isEmpty(childrens)
              && (result = FluentIterable.from(childrens)
                  .filter(children -> children.getMenuConfig().getFieldName() != null))
                      .isEmpty(); childrens =
                          FluentIterable.from(childrens)
                              .transformAndConcat(Menu::getChildrenMenus)) {
          }
          return result;
        }));

    return null;
  }

  // TODO
  // public <T> T toJavaObject(Class<T> type) {
  // Map<String, Field> nameToFields = ClassUtils.getNameToFields(type);
  // cellDatas.forEach((cellData) -> {
  // String key = cellData.getMenu().getCheckMenuConfig().getFieldName();
  // if (nameToFields.containsKey(key)) {
  // jsonData.put(key, cellData.getValue(nameToFields.get(key).getType()));
  // }
  // });
  // return jsonData.toJavaObject(type);
  // }

  public Table getTable() {
    return table;
  }

  public ImmutableListMultimap<Menu, StandardCell> getMenuToCells() {
    return menuToCells;
  }
}
