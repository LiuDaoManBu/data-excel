package com.caotc.excel4j.constant;

import java.util.Optional;
import com.caotc.excel4j.parse.result.Menu;

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
    @Override
    public <T> T castValue(Menu<T> menu) {
      // TODO Auto-generated method stub
      return null;
    }
  },
  MAP {
    @Override
    public <T> T castValue(Menu<T> menu) {
      // TODO Auto-generated method stub
      return null;
    }
  };
  public abstract <T> T castValue(Menu<T> menu);
}
