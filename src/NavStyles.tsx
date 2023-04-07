import { StyleSheet } from "react-native";

export const navContainerViewStyle = (navHeight: string, navBottom: string) => StyleSheet.create({
    container: {
        height: navHeight,
        width: '100%',
        justifyContent: 'center',      
        alignItems: 'center',
        backgroundColor: 'lightgrey',
        position: 'absolute',
        bottom: navBottom,
    }
});

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