import { TextStyle, ViewStyle } from "react-native";

// Move background/text color into the color theme:
const backgroundColor = 'black';
const textColor = 'white';
const justifyContent = 'center';
const alignItems = 'center';

interface ButtonTheme {
    container: ViewStyle,
    text: TextStyle
};

export const navControlButtonTheme: ButtonTheme = {
    container: {
        height: 83,
        width: 83,
        backgroundColor: backgroundColor,
        justifyContent: justifyContent,
        alignItems: alignItems,
        elevation: 1
    },
    text: {
        color: textColor,
        fontSize: 18,        
    }
};

export const cardHeaderButtonTheme: ButtonTheme = {
    container: {
        flex: 1,
        height: '100%',
        maxWidth: 70,
        backgroundColor: backgroundColor,
        justifyContent: justifyContent,
        alignItems: alignItems,
        elevation: 1
    },
    text: {
        color: textColor,
        fontSize: 25,        
    }
};