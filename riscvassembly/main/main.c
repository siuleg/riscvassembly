#include <stdio.h>
#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include "esp_log.h"
#include "sdkconfig.h"

unsigned char get_character() {
  int t;

  for (;;) {
    t = getchar();
    if (t != EOF) {
      return (unsigned char) t;
    }
    vTaskDelay(10 / portTICK_PERIOD_MS);
  }
}

void put_character(unsigned char c) {
  putchar((int) c);
  vTaskDelay(10 / portTICK_PERIOD_MS);
}

void program(void);

void app_main(void)
{
  for (;;) {
    program();
  }
}
