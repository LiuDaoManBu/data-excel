package com.caotc.excel4j.parse.result;

import java.lang.reflect.Field;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.parse.error.MenuError;
import com.google.common.base.Preconditions;
import com.google.common.base.Predicate;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;

public class Menu<V> {
  public static class Builder<V> {
    private StandardCell cell;
    private MenuConfig<V> menuConfig;
    private Table table;
    private Menu<?> parent;

    public Menu<V> build() {
      table = Optional.ofNullable(table).orElse(parent.table);

      // TODO tip
      Preconditions.checkNotNull(cell);
      Preconditions.checkNotNull(table);
      return new Menu<V>(this);
    }

    public StandardCell getCell() {
      return cell;
    }

    public Builder<V> setCell(StandardCell cell) {
      this.cell = cell;
      return this;
    }

    public MenuConfig<V> getMenuConfig() {
      return menuConfig;
    }

    public Builder<V> setMenuConfig(MenuConfig<V> menuConfig) {
      this.menuConfig = menuConfig;
      return this;
    }

    public Table getTable() {
      return table;
    }

    public Builder<V> setTable(Table table) {
      this.table = table;
      return this;
    }

    public Menu<?> getParent() {
      return parent;
    }

    public Builder<V> setParent(Menu<?> parentMenu) {
      this.parent = parentMenu;
      return this;
    }

  }

  private static final Function<MenuConfig<?>, String> MENU_CONFIG_NO_MATCH_MESSAGE_FUNCTION =
      config -> config + "don't have any matches cell";

  public static <V> Builder<V> builder() {
    return new Builder<>();
  }

  private final StandardCell cell;
  private final MenuConfig<V> menuConfig;
  private final ImmutableList<MenuError<V>> errors;
  private final Table table;
  private final Menu<?> parent;
  private final ImmutableList<Menu<?>> childrens;
  private final Data<V> data;

  public Menu(Builder<V> builder) {
    cell = builder.cell;
    menuConfig = builder.menuConfig;
    table = builder.table;
    parent = builder.parent;

    childrens = loadChildrens().map(Builder::build).collect(ImmutableList.toImmutableList());
    data = new Data<V>(this);

    ImmutableCollection<MenuConfig<?>> matchesMenuConfigs =
        childrens.stream().map(Menu::getMenuConfig).collect(ImmutableSet.toImmutableSet());

    // TODO dataError? is MenuError?
    errors = menuConfig.getChildrens().stream()
        .filter(config -> !matchesMenuConfigs.contains(config))
        .map(config -> new MenuError<V>(this, MENU_CONFIG_NO_MATCH_MESSAGE_FUNCTION.apply(config)))
        .collect(ImmutableList.toImmutableList());
  }

  private <T> Stream<Builder<?>> loadChildrens() {
    ImmutableCollection<MenuConfig<?>> childrenConfigs = menuConfig.getChildrens();

    ImmutableList<StandardCell> menuCells =
        menuConfig.getDirection().get(getCell(), menuConfig.getDistance());
    return menuCells.stream().map(cell -> {
      Builder builder = builder().setCell(cell).setParent(this);
      // TODO Duplicate matching?
      MenuConfig<?> config = Iterables.getOnlyElement(childrenConfigs.stream()
          .filter(c -> c.matches(cell)).collect(ImmutableSet.toImmutableSet()));
      return builder.setMenuConfig(config);
    });
  }

  public Optional<Menu<?>> getSuper(Predicate<? super Menu<?>> predicate) {
    Preconditions.checkNotNull(predicate);
    if (isTopMenu()) {
      return Optional.empty();
    }
    return predicate.apply(parent) ? Optional.of(parent) : parent.getSuper(predicate);
  }

  public Optional<Menu<?>> getFieldParent() {
    return getSuper(menu -> menu.getFieldName().isPresent());
  }

  public ImmutableList<Menu<?>> getSubs(Predicate<? super Menu<?>> predicate) {
    Preconditions.checkNotNull(predicate);
    if (isDataMenu()) {
      return ImmutableList.of();
    }
    ImmutableList<Menu<?>> subs =
        childrens.stream().filter(predicate).collect(ImmutableList.toImmutableList());
    return subs.isEmpty() ? childrens.stream().flatMap(menu -> menu.getSubs(predicate).stream())
        .collect(ImmutableList.toImmutableList()) : subs;
  }

  public ImmutableList<Menu<?>> getFieldChildrens() {
    return getSubs(menu -> menu.getFieldName().isPresent());
  }

  public ImmutableList<Field> getFields() {
    if (!getField().isPresent()) {
      return ImmutableList.of();
    }
    com.google.common.collect.ImmutableList.Builder<Field> builder = ImmutableList.builder();
    builder.add(getField().get());
    Optional<Menu<?>> optional = getFieldParent();
    while (optional.isPresent()) {
      Menu<?> menu = optional.get();
      builder.add(menu.getField().get());
      optional = menu.getFieldParent();
    }
    return builder.build().reverse();
  }

  public Optional<StandardCell> nextDataCell(StandardCell cell) {
    if (Objects.isNull(cell)) {
      cell = this.cell;
    }

    Direction direction = menuConfig.getDirection();

    return this.cell.equals(cell) ? direction.getCell(cell, menuConfig.getDistance())
        : direction.nextCell(cell);
  }

  public boolean hasChildren(Menu<?> childrenMenu) {
    return childrens.contains(childrenMenu);
  }

  public boolean hasChildren(StandardCell cell) {
    return childrens.stream().anyMatch(childrenMenu -> childrenMenu.getCell().equals(cell));
  }

  public String getName() {
    return cell.getValueCell().getStringCellValue();
  }

  public Optional<Field> getField() {
    return menuConfig.getField();
  }

  public Optional<String> getFieldName() {
    return menuConfig.getFieldName();
  }

  // delegate methods start
  public boolean isTopMenu() {
    return menuConfig.isTopMenu();
  }

  public boolean isDataMenu() {
    return menuConfig.isDataMenu();
  }

  public boolean isFixedDataMenu() {
    return menuConfig.isFixedDataMenu();
  }

  public boolean isUnFixedDataMenu() {
    return menuConfig.isUnFixedDataMenu();
  }

  public boolean isMustMenu() {
    return menuConfig.isMustMenu();
  }

  public boolean isNotMustMenu() {
    return menuConfig.isNotMustMenu();
  }

  // delegate methods end

  public StandardCell getCell() {
    return cell;
  }

  public MenuConfig<V> getMenuConfig() {
    return menuConfig;
  }

  public Menu<?> getParent() {
    return parent;
  }

  public Table getTable() {
    return table;
  }

  public ImmutableList<Menu<?>> getChildrens() {
    return childrens;
  }

  public Data<V> getData() {
    return data;
  }

  public ImmutableList<MenuError<V>> getErrors() {
    return errors;
  }

}
