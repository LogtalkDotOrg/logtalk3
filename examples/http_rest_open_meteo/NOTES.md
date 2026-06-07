---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.16.7
  kernelspec:
    display_name: Logtalk
    language: logtalk
    name: logtalk_kernel
---

<!--
________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an AS IS BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________
-->

# http_rest_open_meteo

This example shows a small REST client that accesses the Open-Meteo APIs.

The client exposes a single public predicate, `forecast/2`, that takes a
location name as an atom, resolves it through the Open-Meteo geocoding API,
and then fetches a three-day weather forecast for the best match returned by
the service.

The example illustrates four basic steps:

1. Build a geocoding request URL from a location name.
2. Decode the JSON geocoding response and select the first matching result.
3. Build a forecast request URL using the resolved latitude and longitude.
4. Return the resolved location metadata together with the forecast JSON term.

Load the example with:

```logtalk
logtalk_load(http_rest_open_meteo(loader)).
```

Fetch a forecast for a location with a single-word name:

```logtalk
open_meteo_rest_client::forecast(porto, Forecast).
```

Fetch a forecast for a multi-word location name:

```logtalk
open_meteo_rest_client::forecast('New York', Forecast).
```

The returned term has the shape:

```logtalk
forecast(location(Name, Country, Latitude, Longitude, Timezone), ForecastJSON)
```

where `ForecastJSON` is the JSON object returned by the Open-Meteo forecast
API, decoded into the Logtalk JSON term representation.

The example uses these Open-Meteo endpoints:

- `http://geocoding-api.open-meteo.com/v1/search`
- `http://api.open-meteo.com/v1/forecast`

The current `http_client` library only supports `http://` URLs. Although the
Open-Meteo documentation advertises `https://` endpoints, the service also
answers over plain HTTP, which is what this example uses.

The forecast query requests the current `temperature_2m` and `weather_code`
values plus three days of `weather_code`, `temperature_2m_max`, and
`temperature_2m_min` data.

This example depends on network access and on the availability of the
Open-Meteo service. The unit tests use mocked HTTP responses and therefore do
not contact the live service.
