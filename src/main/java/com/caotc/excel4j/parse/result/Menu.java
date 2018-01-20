package com.caotc.excel4j.parse.result;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Stream;
import com.caotc.excel4j.config.MenuConfig;
import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.matcher.data.type.BaseDataType;
import com.caotc.excel4j.parse.error.ValidationError;
import com.google.common.base.Preconditions;
import com.google.common.base.Predicate;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;

public class Menu {
  public static class Builder {
    private StandardCell cell;
    private MenuConfig config;
    private Table table;
    private Menu parent;

    public Menu build() {
      return new Menu(this);
    }

    public StandardCell getCell() {
      return cell;
    }

    public Builder setCell(StandardCell cell) {
      this.cell = cell;
      return this;
    }

    public MenuConfig getConfig() {
      return config;
    }

    public Builder setConfig(MenuConfig config) {
      this.config = config;
      return this;
    }

    public Table getTable() {
      return table;
    }

    public Builder setTable(Table table) {
      this.table = table;
      return this;
    }

    public Menu getParent() {
      return parent;
    }

    public Builder setParent(Menu parent) {
      this.parent = parent;
      return this;
    }

  }

  private final Function<MenuConfig, String> MENU_CONFIG_NO_MATCH_MESSAGE_FUNCTION =
      config -> config + "don't have any matches cell";

  public static Builder builder() {
    return new Builder();
  }

  private final StandardCell cell;
  private final MenuConfig config;
  private final ImmutableList<ValidationError<Menu>> errors;
  private final Table table;
  private final Menu parent;
  private final ImmutableList<Menu> childrens;
  private final MenuData data;

  public Menu(Builder builder) {
    cell = builder.cell;
    config = builder.config;
    parent = builder.parent;
    table = Optional.ofNullable(builder.table)
        .orElse(Optional.ofNullable(parent).map(Menu::getTable).orElse(null));
    childrens =
        loadChildrens().peek(childrenBuilder -> childrenBuilder.setParent(this).setTable(table))
            .map(Builder::build).collect(ImmutableList.toImmutableList());
    data = new MenuData(this);

    ImmutableCollection<MenuConfig> matchesMenuConfigs =
        childrens.stream().map(Menu::getConfig).collect(ImmutableSet.toImmutableSet());

    // TODO dataError? is MenuError?
    errors = config.getChildrens().stream().filter(config -> !matchesMenuConfigs.contains(config))
        .map(config -> new ValidationError<Menu>(this,
            MENU_CONFIG_NO_MATCH_MESSAGE_FUNCTION.apply(config)))
        .collect(ImmutableList.toImmutableList());

    // TODO tip
    Preconditions.checkNotNull(cell);
    Preconditions.checkNotNull(table);
  }

  private Stream<Builder> loadChildrens() {
    ImmutableCollection<MenuConfig> childrenConfigs = config.getChildrens();

    if (!childrenConfigs.isEmpty()) {
      ImmutableList<StandardCell> menuCells =
          config.getDirection().get(getCell(), config.getDistance());
      return menuCells.stream().map(cell -> {
        Builder builder = builder().setCell(cell).setParent(this);
        // TODO Duplicate matching?
        MenuConfig config = Iterables.getOnlyElement(childrenConfigs.stream()
            .filter(c -> c.matches(cell)).collect(ImmutableSet.toImmutableSet()));
        return builder.setConfig(config);
      });
    } else {
      return Stream.empty();
    }
  }

  public Optional<Menu> getSuper(Predicate<? super Menu> predicate) {
    Preconditions.checkNotNull(predicate);
    if (isTopMenu()) {
      return Optional.empty();
    }
    return predicate.apply(parent) ? Optional.of(parent) : parent.getSuper(predicate);
  }

  public Optional<Menu> getFieldParent() {
    return getSuper(menu -> menu.getFieldName().isPresent());
  }

  public ImmutableList<Menu> getSubs(Predicate<? super Menu> predicate) {
    Preconditions.checkNotNull(predicate);
    if (isDataMenu()) {
      return ImmutableList.of();
    }
    ImmutableList<Menu> subs =
        childrens.stream().filter(predicate).collect(ImmutableList.toImmutableList());
    return subs.isEmpty() ? childrens.stream().flatMap(menu -> menu.getSubs(predicate).stream())
        .collect(ImmutableList.toImmutableList()) : subs;
  }

  public ImmutableList<Menu> getFieldChildrens() {
    return getSubs(menu -> menu.getFieldName().isPresent());
  }

  public ImmutableList<Field> getFields() {
    if (!getField().isPresent()) {
      return ImmutableList.of();
    }
    com.google.common.collect.ImmutableList.Builder<Field> builder = ImmutableList.builder();
    builder.add(getField().get());
    Optional<Menu> optional = getFieldParent();
    while (optional.isPresent()) {
      Menu menu = optional.get();
      builder.add(menu.getField().get());
      optional = menu.getFieldParent();
    }
    return builder.build().reverse();
  }

  public Optional<StandardCell> nextDataCell(StandardCell cell) {
    if (Objects.isNull(cell)) {
      cell = this.cell;
    }

    Direction direction = config.getDirection();

    return this.cell.equals(cell) ? direction.getCell(cell, config.getDistance())
        : direction.nextCell(cell);
  }

  public boolean hasChildren(Menu childrenMenu) {
    return childrens.contains(childrenMenu);
  }

  public boolean hasChildren(StandardCell cell) {
    return childrens.stream().anyMatch(childrenMenu -> childrenMenu.getCell().equals(cell));
  }

  public ImmutableList<ValidationError<Menu>> getAllErrors() {
    return Stream
        .concat(errors.stream(),
            childrens.stream().map(Menu::getAllErrors).flatMap(Collection::stream)
                .map(error -> new ValidationError<Menu>(this, error.getMessage())))
        .collect(ImmutableList.toImmutableList());
  }

  public String getName() {
    return BaseDataType.STRING.cast(cell.getValueCell(), String.class);
  }

  public Optional<Field> getField() {
    return config.getField();
  }

  public Optional<String> getFieldName() {
    return config.getFieldName();
  }

  // delegate methods start
  public boolean isTopMenu() {
    return config.isTopMenu();
  }

  public boolean isDataMenu() {
    return config.isDataMenu();
  }

  public boolean isSingleDataMenu() {
    return config.isSingleDataMenu();
  }

  // public boolean isFixedDataMenu() {
  // return menuConfig.isFixedDataMenu();
  // }

  public boolean isUnFixedDataMenu() {
    return config.isUnFixedDataMenu();
  }

  public boolean isMustMenu() {
    return config.isMustMenu();
  }

  public boolean isNotMustMenu() {
    return config.isNotMustMenu();
  }

  // delegate methods end

  public StandardCell getCell() {
    return cell;
  }

  public MenuConfig getConfig() {
    return config;
  }

  public Menu getParent() {
    return parent;
  }

  public Table getTable() {
    return table;
  }

  public ImmutableList<Menu> getChildrens() {
    return childrens;
  }

  public MenuData getData() {
    return data;
  }

  public ImmutableList<ValidationError<Menu>> getErrors() {
    return errors;
  }

}
