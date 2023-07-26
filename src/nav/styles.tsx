import { StyleSheet } from "react-native";

import { colorTheme, borderTheme } from "../theme/theme";

export const navContainerViewStyle = (isNavOpened: boolean) => {
    // Used to hide and show part of the Nav; this mimics the Nav being open or
    // closed.
    let navHeight = '50%';
    let navBottom = isNavOpened ? '0%' : '-42%';
    
    return StyleSheet.create({
        container: {
            height: navHeight,
            width: '100%',
            justifyContent: 'center',      
            alignItems: 'center',
            backgroundColor: colorTheme.component.background,
            position: 'absolute',
            bottom: navBottom,
            borderTopColor: borderTheme.component.borderColor,
            borderTopWidth: borderTheme.component.borderWidth         
        }
    });
}

export const navContainerBarViewStyle = StyleSheet.create({
    container: {
        flex: 1,
        width: '100%',
        justifyContent: 'center',      
        alignItems: 'flex-end',
        position: 'absolute',
        top: 2.5
    }
});