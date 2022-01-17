<!DOCTYPE qgis PUBLIC 'http://mrcc.com/qgis.dtd' 'SYSTEM'>
<qgis styleCategories="AllStyleCategories" version="3.22.2-Białowieża" hasScaleBasedVisibilityFlag="0" maxScale="0" minScale="1e+08">
  <flags>
    <Identifiable>1</Identifiable>
    <Removable>1</Removable>
    <Searchable>1</Searchable>
    <Private>0</Private>
  </flags>
  <temporal mode="0" enabled="0" fetchMode="0">
    <fixedRange>
      <start></start>
      <end></end>
    </fixedRange>
  </temporal>
  <customproperties>
    <Option type="Map">
      <Option type="QString" value="Value" name="identify/format"/>
    </Option>
  </customproperties>
  <pipe-data-defined-properties>
    <Option type="Map">
      <Option type="QString" value="" name="name"/>
      <Option name="properties"/>
      <Option type="QString" value="collection" name="type"/>
    </Option>
  </pipe-data-defined-properties>
  <pipe>
    <provider>
      <resampling enabled="false" zoomedInResamplingMethod="nearestNeighbour" zoomedOutResamplingMethod="nearestNeighbour" maxOversampling="2"/>
    </provider>
    <rasterrenderer opacity="1" type="paletted" nodataColor="" band="1" alphaBand="-1">
      <rasterTransparency/>
      <minMaxOrigin>
        <limits>None</limits>
        <extent>WholeRaster</extent>
        <statAccuracy>Estimated</statAccuracy>
        <cumulativeCutLower>0.02</cumulativeCutLower>
        <cumulativeCutUpper>0.98</cumulativeCutUpper>
        <stdDevFactor>2</stdDevFactor>
      </minMaxOrigin>
      <colorPalette>
        <paletteEntry color="#d7191c" value="0" label="never" alpha="0"/>
        <paletteEntry color="#ed6e43" value="1" label="2015" alpha="255"/>
        <paletteEntry color="#feba6e" value="10" label="2016" alpha="255"/>
        <paletteEntry color="#ffe8a4" value="11" label="2015-2016" alpha="255"/>
        <paletteEntry color="#e7f6b8" value="100" label="2017" alpha="255"/>
        <paletteEntry color="#b7e2a8" value="101" label="2015 + 2017" alpha="255"/>
        <paletteEntry color="#74b7ae" value="110" label="2016-2017" alpha="255"/>
        <paletteEntry color="#2b83ba" value="111" label="2015-2017" alpha="255"/>
      </colorPalette>
      <colorramp type="gradient" name="[source]">
        <Option type="Map">
          <Option type="QString" value="215,25,28,255" name="color1"/>
          <Option type="QString" value="43,131,186,255" name="color2"/>
          <Option type="QString" value="0" name="discrete"/>
          <Option type="QString" value="gradient" name="rampType"/>
          <Option type="QString" value="0.25;253,174,97,255:0.5;255,255,191,255:0.75;171,221,164,255" name="stops"/>
        </Option>
        <prop k="color1" v="215,25,28,255"/>
        <prop k="color2" v="43,131,186,255"/>
        <prop k="discrete" v="0"/>
        <prop k="rampType" v="gradient"/>
        <prop k="stops" v="0.25;253,174,97,255:0.5;255,255,191,255:0.75;171,221,164,255"/>
      </colorramp>
    </rasterrenderer>
    <brightnesscontrast gamma="1" brightness="0" contrast="0"/>
    <huesaturation colorizeGreen="128" saturation="0" colorizeOn="0" colorizeBlue="128" colorizeRed="255" invertColors="0" colorizeStrength="100" grayscaleMode="0"/>
    <rasterresampler maxOversampling="2"/>
    <resamplingStage>resamplingFilter</resamplingStage>
  </pipe>
  <blendMode>0</blendMode>
</qgis>
