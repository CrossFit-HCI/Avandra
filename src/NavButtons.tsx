import React from "react";
import { closeNav, useAppDispatch } from "./NavState";
import { Button, GestureResponderEvent } from "react-native";

interface NavButtonProps {
    title: string;
    onPress: (event: GestureResponderEvent) => void;
}

const NaviButton = (props:NavButtonProps) => {
    const dispatch = useAppDispatch();
    const closeTheNav = () => dispatch(closeNav())
    
    const onPressCallback = (event: GestureResponderEvent) => {
        closeTheNav();
        props.onPress(event);
    }

    return (
        <Button title={props.title} onPress={onPressCallback} />
    )
}

export default NaviButton;