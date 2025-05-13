SELECT
  z.geocodigo::double precision AS geocodigo,
  c.nom_comuna,  -- edad por intervalos 
    COUNT(*) FILTER (WHERE p.p09 < 30) AS edad_menor_30,
	COUNT(*) FILTER (WHERE p.p09 BETWEEN 30 AND 40) AS edad_30_40,
	COUNT(*) FILTER (WHERE p.p09 BETWEEN 40 AND 50) AS edad_40_50, 
	COUNT(*) FILTER (WHERE p.p09 BETWEEN 50 AND 60) AS edad_50_60, 
	COUNT(*) FILTER (WHERE p.p09 BETWEEN 60 AND 70) AS edad_60_70, 
	COUNT(*) FILTER (WHERE p.p09 BETWEEN 70 AND 80) AS edad_70_80, 
	COUNT(*) FILTER (WHERE p.p09 > 80) AS edad_mayor_80, 

-- rango de escolaridad
	COUNT (*) FILTER (WHERE p.escolaridad = 0) AS esc_0, 
	COUNT (*) FILTER (WHERE p.escolaridad BETWEEN 1 AND 8) AS esc_1_8,
	COUNT (*) FILTER (WHERE p.escolaridad BETWEEN 8 AND 12) AS esc_8_12,
	COUNT (*) FILTER (WHERE p.escolaridad > 12) AS esc_mayor_12,
--	COUNT (*) FILTER (WHERE p.escolaridad = 27 AND p.esc = 99) AS esc_27_99,

-- sexo
	COUNT (*) FILTER (WHERE p.p08 = 1) AS sexo_m,
	COUNT (*) FILTER (WHERE p.p08 = 2) AS sexo_f
	--COUNT (*) FILTER (WHERE p.p08 = 3) AS perdido,
	--COUNT (*) FILTER (WHERE p.p08 = 0) AS no_aplica,
FROM personas AS p
JOIN hogares  AS h ON p.hogar_ref_id    = h.hogar_ref_id
JOIN viviendas AS v ON h.vivienda_ref_id = v.vivienda_ref_id
JOIN zonas AS z ON v.zonaloc_ref_id   = z.zonaloc_ref_id
JOIN comunas AS c ON z.codigo_comuna    = c.codigo_comuna
GROUP BY z.geocodigo, c.nom_comuna;
