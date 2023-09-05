import { StyleSheet } from "react-native";

import { colorTheme, fontTheme, cardTheme } from "../theme/theme";

export const forecastStyle = StyleSheet.create({
        component: {
            flex: 1,
            justifyContent: 'center',
            alignItems: 'center',
            backgroundColor: colorTheme.component.background,
            card: cardTheme
        },

        heading1: {
            color: fontTheme.heading1Font.color,
            fontSize: fontTheme.heading1Font.size
        }
    });
