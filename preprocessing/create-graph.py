from collections import defaultdict
graph = defaultdict(set)

red_line = "RM-EN-EP-NB-BK-AS-MRA-MA-19-12-OW-EM-MT-PL-CC-16-24-GP-BP-DC-CM-SS-SB-SFC-MB"
orange_line = "RM-EN-EP-NB-BK-AS-MRA-MA-19-12-LM-FV-CL-SL-BF-BHC-HY-SH-UC-FM-WS-ML-BE"
yellow_line = "AN-PC-WP-NC-CN-PH-WC-LF-OR-RR-MRA-MA-19-12-OW-EM-MT-PL-CC-16-24-GP-BP-DC-CM-SS-SB-SFC-MB"
green_line = "BE-ML-WS-FM-UC-SH-HY-BHC-BF-SL-CL-FV-LM-OW-EM-MT-PL-CC-16-24-GP-BP-DC"
blue_line = "ED-WD-CV-BHC-BF-SL-CL-FV-LM-OW-EM-MT-PL-CC-16-24-GP-BP-DC"

for line in [red_line, orange_line, yellow_line, green_line, blue_line]:
    stations = line.split("-")
    for i in range(1, len(stations)):
        graph[stations[i-1]].add(stations[i])
        graph[stations[i]].add(stations[i-1])

print(graph)
