package com.caotc.excel4j.constant;

import java.util.Optional;
import com.caotc.excel4j.parse.result.Menu;
import com.google.common.reflect.TypeToken;

public enum CastType {
  OBJECT {
    @Override
    public <T> T castValue(Menu<T> menu) {
      Optional<T> optional = menu.getMenuConfig().getParserConfig()
          .newInstance(menu.getData().getDataConfig().getFieldType());
      T value = optional.get();
      menu.getFieldChildrens().forEach(children -> {

      });
      return null;
    }
  },
  COLLECTION {
  },
  MAP {
  };
  public abstract <T> T castValue(Menu<T> menu);
  public abstract <T> T castValue(TypeToken<T> targetType);
}
