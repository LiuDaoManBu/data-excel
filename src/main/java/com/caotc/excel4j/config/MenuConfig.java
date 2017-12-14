package com.caotc.excel4j.config;

import java.lang.reflect.Field;
import java.util.Collection;
import java.util.Objects;
import java.util.Optional;
import org.apache.commons.collections4.CollectionUtils;
import com.caotc.excel4j.constant.Direction;
import com.caotc.excel4j.constant.LoadType;
import com.caotc.excel4j.constant.MenuNecessity;
import com.caotc.excel4j.constant.MenuType;
import com.caotc.excel4j.matcher.usermodel.StandardCellMatcher;
import com.caotc.excel4j.parse.result.StandardCell;
import com.google.common.base.Preconditions;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.Iterables;

public class MenuConfig<V> {
  public static class Builder<V> {
    private TableConfig tableConfig;
    // 菜单匹配器
    private StandardCellMatcher menuMatcher;
    // 第一个数据单元格相对于菜单单元格的单元格距离
    private int distance;
    private MenuNecessity menuNecessity;
    private Direction direction;
    private MenuType menuType;
    private MenuConfig<?> parentMenuConfig;
    private ImmutableCollection<MenuConfig<?>> childrenMenuConfigs;
    private DataConfig<V> dataConfig;
    private ParserConfig parserConfig;

    public MenuConfig<V> build() {
      Preconditions.checkState(Objects.nonNull(tableConfig) || Objects.nonNull(parentMenuConfig));
      Preconditions.checkNotNull(menuMatcher);
      Preconditions.checkNotNull(menuNecessity);
      Preconditions.checkState(Objects.nonNull(direction) || Objects.nonNull(parentMenuConfig));
      Preconditions.checkNotNull(menuType);
      Preconditions
          .checkState(CollectionUtils.isEmpty(childrenMenuConfigs) || Objects.nonNull(dataConfig));
      // TODO

      direction = Optional.ofNullable(direction).orElse(parentMenuConfig.direction);
      tableConfig = Optional.ofNullable(tableConfig).orElse(parentMenuConfig.tableConfig);
      parserConfig = Optional.ofNullable(parserConfig).orElse(ParserConfig.GLOBAL);
      return new MenuConfig<V>(this);
    }

    public TableConfig getTableConfig() {
      return tableConfig;
    }

    public Builder<V> setTableConfig(TableConfig tableConfig) {
      this.tableConfig = tableConfig;
      return this;
    }

    public StandardCellMatcher getMenuMatcher() {
      return menuMatcher;
    }

    public Builder<V> setMenuMatcher(StandardCellMatcher menuMatcher) {
      this.menuMatcher = menuMatcher;
      return this;
    }

    public int getDistance() {
      return distance;
    }

    public Builder<V> setDistance(int distance) {
      this.distance = distance;
      return this;
    }

    public MenuNecessity getMenuNecessity() {
      return menuNecessity;
    }

    public Builder<V> setMenuNecessity(MenuNecessity menuNecessity) {
      this.menuNecessity = menuNecessity;
      return this;
    }

    public Direction getDirection() {
      return direction;
    }

    public Builder<V> setDirection(Direction direction) {
      this.direction = direction;
      return this;
    }

    public MenuType getMenuType() {
      return menuType;
    }

    public Builder<V> setMenuType(MenuType menuType) {
      this.menuType = menuType;
      return this;
    }

    public MenuConfig<?> getParentMenuConfig() {
      return parentMenuConfig;
    }

    public Builder<V> setParentMenuConfig(MenuConfig<?> parentMenuConfig) {
      this.parentMenuConfig = parentMenuConfig;
      return this;
    }

    public ImmutableCollection<MenuConfig<?>> getChildrenMenuConfigs() {
      return childrenMenuConfigs;
    }

    public Builder<V> setChildrenMenuConfigs(ImmutableCollection<MenuConfig<?>> childrenMenuConfigs) {
      this.childrenMenuConfigs = childrenMenuConfigs;
      return this;
    }

    public DataConfig<V> getDataConfig() {
      return dataConfig;
    }

    public Builder<V> setDataConfig(DataConfig<V> dataConfig) {
      this.dataConfig = dataConfig;
      return this;
    }

  }

  private static final int DEFAULT_DISTANCE = 1;
  private static final MenuNecessity DEFAULT_MENU_NECESSITY = MenuNecessity.MUST;

  public static <V> Builder<V> builder() {
    return new Builder<V>().setDistance(DEFAULT_DISTANCE).setMenuNecessity(DEFAULT_MENU_NECESSITY);
  }

  private final TableConfig tableConfig;
  // 菜单匹配器
  private final StandardCellMatcher menuMatcher;
  // 第一个数据单元格相对于菜单单元格的单元格距离
  private final int distance;
  private final MenuNecessity menuNecessity;
  private final Direction direction;
  private final MenuType menuType;
  private final MenuConfig<?> parentMenuConfig;
  private final ImmutableCollection<MenuConfig<?>> childrenMenuConfigs;
  private final DataConfig<V> dataConfig;
  private final ParserConfig parserConfig;

  public MenuConfig(Builder<V> builder) {
    tableConfig = builder.tableConfig;
    menuMatcher = builder.menuMatcher;
    distance = builder.distance;
    menuNecessity = builder.menuNecessity;
    direction = builder.direction;
    menuType = builder.menuType;
    parentMenuConfig = builder.parentMenuConfig;
    childrenMenuConfigs = builder.childrenMenuConfigs;
    dataConfig = builder.dataConfig;
    parserConfig=builder.parserConfig;
  }

  public Optional<Field> getField() {
    return Optional.ofNullable(dataConfig).map(DataConfig::getField);
  }

  public Optional<String> getFieldName() {
    return Optional.ofNullable(dataConfig).map(DataConfig::getFieldName);
  }

  public boolean isTopMenu() {
    return parentMenuConfig == null;
  }

  public boolean isMustMenu() {
    return MenuNecessity.MUST.equals(menuNecessity);
  }

  public boolean isNotMustMenu() {
    return MenuNecessity.NOT_MUST.equals(menuNecessity);
  }

  public boolean isDataMenu() {
    return Iterables.isEmpty(childrenMenuConfigs);
  }

  public boolean isFixedDataMenu() {
    return isDataMenu() && LoadType.FIXED.equals(dataConfig.getLoadType());
  }

  public boolean isUnFixedDataMenu() {
    return isDataMenu() && LoadType.UNFIXED.equals(dataConfig.getLoadType());
  }

  public boolean isMixedDataMenu() {
    return isDataMenu() && LoadType.MIXED.equals(dataConfig.getLoadType());
  }

  // delegate methods start

  public boolean matches(StandardCell cell) {
    return menuMatcher.test(cell);
  }

  public boolean matches(Object value) {
    return dataConfig.matches(value);
  }

  public boolean support(Object value) {
    return dataConfig.support(value);
  }

  public Collection<Class<?>> canCastClasses() {
    return dataConfig.canCastClasses();
  }

  public <R> boolean canCast(Class<R> clazz) {
    return dataConfig.canCast(clazz);
  }

  public <R> R cast(Object value, Class<R> clazz) {
    return dataConfig.cast(value, clazz);
  }
  // delegate methods end

  public MenuType getMenuType() {
    return menuType;
  }

  public StandardCellMatcher getMenuMatcher() {
    return menuMatcher;
  }

  public int getDistance() {
    return distance;
  }

  public MenuConfig<?> getParentMenuConfig() {
    return parentMenuConfig;
  }

  public Direction getDirection() {
    return direction;
  }

  public MenuNecessity getMenuNecessity() {
    return menuNecessity;
  }

  public DataConfig<V> getDataConfig() {
    return dataConfig;
  }

  public ImmutableCollection<MenuConfig<?>> getChildrenMenuConfigs() {
    return childrenMenuConfigs;
  }

  public TableConfig getTableConfig() {
    return tableConfig;
  }

  public ParserConfig getParserConfig() {
    return parserConfig;
  }

}
