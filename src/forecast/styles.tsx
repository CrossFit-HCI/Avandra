import { StyleSheet } from "react-native";

import { colorTheme, fontTheme } from "../theme/theme";

export const forecastStyle = StyleSheet.create({
        component: {
            flex: 1,
            flexDirection: 'column',
            justifyContent: 'center',
            alignItems: 'center',
            backgroundColor: colorTheme.component.background
        },

        heading1: {
            color: fontTheme.heading1Font.color,
            fontSize: fontTheme.heading1Font.size,
        }
    });